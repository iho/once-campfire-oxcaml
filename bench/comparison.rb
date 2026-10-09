require "json"

module Comparison
  ROUTES = %w[room_show messages_page sidebar search post_message].freeze

  def self.cpus(value)
    numbers = value.split(",", -1).flat_map do |part|
      raise ArgumentError, "invalid CPU set" unless part.match?(/\A\d+(?:-\d+)?\z/)
      first, last = part.split("-").map(&:to_i)
      last ||= first
      raise ArgumentError, "invalid CPU range" if last < first || last - first > 4096
      (first..last).to_a
    end
    raise ArgumentError, "empty or overlapping CPU set" if numbers.empty? || numbers.uniq != numbers
    numbers
  end

  def self.check_sample(sample)
    unless sample.fetch("errors").zero? && sample.fetch("invalid_responses").zero? &&
        sample.fetch("validation") == "route-contract-v1" && sample.fetch("ok").positive? &&
        sample.fetch("statuses") == { "200" => sample.fetch("ok") }
      raise "Invalid benchmark sample: #{sample}"
    end
    sample
  end

  def self.summary(rows, rounds)
    %w[oxcaml rust].to_h do |app|
      selected = rows.select { |row| row.fetch(:app) == app }
      raise "incomplete rounds for #{app}" unless selected.map { |row| row.fetch(:round) }.sort == (1..rounds).to_a
      values = ROUTES.to_h do |route|
        rates = selected.map do |row|
          samples = row.fetch(:samples).select { |sample| sample.fetch("route") == route }
          raise "missing or duplicate route" unless samples.size == 1
          check_sample(samples.first).fetch("rps")
        end
        sorted = rates.sort
        median = (sorted[(sorted.size - 1) / 2] + sorted[sorted.size / 2]) / 2.0
        [route, { median_rps: median, runs: rates }]
      end
      [app, values]
    end
  end
end
