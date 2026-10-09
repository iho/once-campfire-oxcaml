# Linux-only head-to-head; shared upstream contracts/loadgen remain unchanged.
require "optparse"
require "digest"
require "fileutils"
require "find"
require "securerandom"
require "time"
require_relative "comparison"

options = { rounds: 4, duration: 8, concurrency: 16, cpus: "0-1", client_cpus: "2-3",
  port: 25130, output: "tmp/head-to-head", rust_image: "campfire-rust:comparison",
  oxcaml_image: "campfire-oxcaml:comparison", verification: nil, seed: nil }
OptionParser.new do |parser|
  options.each do |key, default|
    parser.on("--#{key.to_s.tr('_', '-')} VALUE", default.is_a?(Integer) ? Integer : String) do |value|
      options[key] = value
    end
  end
end.parse!
abort "Linux host required for server/client CPU affinity" unless RUBY_PLATFORM.include?("linux")
abort "--verification and --seed required" unless options[:verification] && options[:seed]
abort "positive duration/concurrency and even rounds >= 2 required" unless options[:rounds] >= 2 &&
  options[:rounds].even? && options[:duration].positive? && options[:concurrency].positive?
server_cpus, client_cpus = options.values_at(:cpus, :client_cpus).map { |set| Comparison.cpus(set) }
abort "server and client CPU sets overlap" unless (server_cpus & client_cpus).empty?
verification, seed, output = options.values_at(:verification, :seed, :output).map { |path| File.expand_path(path) }
abort "output already exists" if File.exist?(output)
require File.join(verification, "bench/contracts")
require File.join(verification, "bench/validate_acks")
require File.join(verification, "bench/http_client")
include BenchmarkSupport
lock = File.open("/tmp/once-campfire-verification-benchmark.lock", "a")
abort "another comparison is running" unless lock.flock(File::LOCK_EX | File::LOCK_NB)
FileUtils.mkdir_p(output)
loadgen = File.join(verification, "loadgen/target/release/loadgen")
labels = JSON.parse(File.read(File.join(seed, "labels.json")))
seed_db = File.join(seed, "db/production.sqlite3")
seed_hash = Digest::SHA256.file(seed_db).hexdigest
fixture_env = File.readlines(File.join(seed, "reference.env"), chomp: true)
  .reject { |line| line.empty? || line.start_with?("#") }.to_h { |line| line.split("=", 2) }
images = %w[oxcaml rust].to_h do |app|
  [app, run("docker", "image", "inspect", "-f", "{{.Id}}", options.fetch(:"#{app}_image")).strip]
end
metadata = { started_at: Time.now.utc.iso8601, seed_sha256: seed_hash, images: images,
  verification_revision: run("git", "-C", verification, "rev-parse", "HEAD").strip,
  oxcaml_revision: run("git", "rev-parse", "HEAD").strip,
  oxcaml_dirty: !run("git", "status", "--porcelain").strip.empty?,
  image_labels: images.transform_values { |id| JSON.parse(run("docker", "image", "inspect", "-f", "{{json .Config.Labels}}", id)) },
  loadgen_sha256: Digest::SHA256.file(loadgen).hexdigest, host: RUBY_PLATFORM,
  server_cpus: server_cpus, client_cpus: client_cpus, rounds: options[:rounds],
  duration: options[:duration], concurrency: options[:concurrency], complete: false }
write_json(File.join(output, "metadata.json"), metadata)
sql = ->(db, query) { JSON.parse(run("sqlite3", "-cmd", ".timeout 10000", "-readonly", "-json", db, query)) }
lg = ->(*args) { JSON.parse(run("taskset", "-c", options[:client_cpus], loadgen, *args)) }
rows = []
options[:rounds].times do |index|
  (index.even? ? %w[oxcaml rust] : %w[rust oxcaml]).each do |app|
    name = "cf-head-to-head-#{Process.pid}-#{SecureRandom.hex(4)}"
    data = File.expand_path("../runtime/#{name}", output)
    FileUtils.mkdir_p(File.join(data, "db"))
    FileUtils.mkdir_p(File.join(data, "logs"))
    db = File.join(data, "db/production.sqlite3")
    run("sqlite3", seed_db, ".backup '#{db.gsub("'", "''")}'")
    FileUtils.cp_r(File.join(seed, "storage"), File.join(data, "files"))
    Find.find(data) do |entry|
      raise "unexpected fixture symlink" if File.symlink?(entry)
      stat = File.stat(entry)
      File.chmod((stat.mode & 0o7777) | (stat.directory? ? 0o2070 : 0o0060), entry)
    end
    run("sqlite3", db, "UPDATE webhooks SET url='http://127.0.0.1:9/hook/'||id; UPDATE push_subscriptions SET endpoint='https://127.0.0.1:9/push/'||id;")
    initial = sql.call(db, "SELECT COUNT(*) AS n FROM messages").first.fetch("n")
    row = { app: app, round: index + 1, samples: [], warmups: [] }
    base = "http://127.0.0.1:#{options[:port]}"
    config = fixture_env.merge("HTTP_PORT" => options[:port].to_s, "TARGET_PORT" => (options[:port] + 1).to_s,
      "CAMPFIRE_STORAGE_PATH" => "/rails/storage", "RAILS_LOG_LEVEL" => "warn",
      "WEB_WORKERS" => server_cpus.size.to_s, "RAILS_MAX_THREADS" => server_cpus.size.to_s,
      "JOB_CONCURRENCY" => "1")
    begin
      run("docker", "run", "--detach", "--name", name, "--network", "host", "--cpuset-cpus", options[:cpus],
        "--group-add", Process.gid.to_s, *environment(config),
        *mounts(data => "/rails/storage"), images.fetch(app))
      client = BenchmarkHTTPClient.new(base)
      deadline = clock + 60
      until client.ready?
        raise "#{app} startup timeout" if clock > deadline
        sleep 0.1
      end
      cookie = lg.call("login", "--base", base, "--email", labels.fetch("emails.david"), "--password", labels.fetch("passwords.all")).fetch("cookie")
      room, write_room = labels.values_at("rooms.watercooler", "rooms.hq").map { |value| Integer(value) }
      scraped = lg.call("scrape", "--base", base, "--cookie", cookie, "--room", room.to_s)
      prepared = BenchmarkContracts.prepare(base, cookie, db, labels, scraped.fetch("css"), File.join(data, "contracts"))
      row[:preflight] = prepared.fetch(:preflight)
      row[:sqlite] = sql.call(db, "PRAGMA journal_mode").first
      paths = { "room_show" => "/rooms/#{room}", "messages_page" => "/rooms/#{room}/messages?before=#{labels.fetch('messages.busy_060')}",
        "sidebar" => "/users/me/sidebar", "search" => "/searches?q=coffee", "post_message" => nil }
      acknowledged, audits = 0, []
      paths.each do |route, path|
        args = path ? ["--path", path] : ["--post-room", write_room.to_s, "--csrf", scraped.fetch("csrf").to_s]
        [[:warmups, 2], [:samples, options[:duration]]].each do |phase, duration|
          audit_args = []
          unless path
            audit = File.join(output, "#{app}-#{index + 1}-#{phase}-writes.jsonl")
            audits << audit
            audit_args = ["--audit-writes", audit]
          end
          sample = Comparison.check_sample(lg.call("http", "--base", base, "--cookie", cookie,
            *args, "--validate", prepared.fetch(:contracts).fetch(route), *audit_args,
            "--conc", options[:concurrency].to_s, "--duration", duration.to_s))
          acknowledged += sample.fetch("ok") unless path
          row.fetch(phase) << sample.merge("route" => route)
          puts "#{app} round #{index + 1} #{phase} #{route}: #{sample.fetch('rps')} req/s"
          STDOUT.flush
          write_json(File.join(output, "#{app}-#{index + 1}.json"), row)
        end
      end
      persisted = sql.call(db, "SELECT COUNT(*) AS n FROM messages").first.fetch("n") - initial
      raise "acknowledged/persisted count mismatch" unless persisted == acknowledged
      row[:write_audit] = AcknowledgedWrites.verify(db, write_room, audits, acknowledged)
      raise "database integrity failed" unless sql.call(db, "PRAGMA integrity_check").first.values == ["ok"]
      write_json(File.join(output, "#{app}-#{index + 1}.json"), row)
      rows << row
    ensure
      logs, = Open3.capture2e("docker", "logs", name)
      File.write(File.join(output, "#{app}-#{index + 1}.log"), logs)
      remove_container(name)
    end
  end
end
raise "original fixture changed" unless Digest::SHA256.file(seed_db).hexdigest == seed_hash
metadata[:complete] = true
write_json(File.join(output, "summary.json"), metadata: metadata, results: Comparison.summary(rows, options[:rounds]))
