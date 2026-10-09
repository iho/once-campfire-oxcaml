require "minitest/autorun"
require_relative "comparison"

class ComparisonTest < Minitest::Test
  def sample
    { "errors" => 0, "invalid_responses" => 0, "validation" => "route-contract-v1",
      "ok" => 10, "statuses" => { "200" => 10 }, "rps" => 10.0 }
  end

  def test_cpu_sets
    assert_equal [0, 1, 4], Comparison.cpus("0-1,4")
    ["", "0,,1", "2-1", "0-2,2", "-1", "a", "1-5000"].each do |value|
      assert_raises(ArgumentError) { Comparison.cpus(value) }
    end
  end

  def test_samples_require_success_and_actual_contract_validation
    assert_equal sample, Comparison.check_sample(sample)
    [{ "errors" => 1 }, { "invalid_responses" => 1 }, { "validation" => nil },
      { "ok" => 0 }, { "statuses" => { "200" => 9, "302" => 1 } }].each do |change|
      assert_raises(RuntimeError) { Comparison.check_sample(sample.merge(change)) }
    end
  end

  def test_summary_requires_every_route_and_round
    rows = %w[oxcaml rust].flat_map do |app|
      [1, 2].map { |round| { app: app, round: round,
        samples: Comparison::ROUTES.map { |route| sample.merge("route" => route, "rps" => round * 10.0) } } }
    end
    assert_equal 15.0, Comparison.summary(rows, 2).fetch("oxcaml").fetch("search").fetch(:median_rps)
    assert_raises(RuntimeError) { Comparison.summary(rows.drop(1), 2) }
    rows.first.fetch(:samples).pop
    assert_raises(RuntimeError) { Comparison.summary(rows, 2) }
  end
end
