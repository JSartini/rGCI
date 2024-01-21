test_that("user can pass periodogram arguments to GCI calculation procedure", {
  baseline = baseline_metrics %>%
    filter(ID == "full_ma") %>%
    select(GCI) %>%
    as.numeric() %>%
    unname()

  # Stddevs accommodate for frequency unit shift in old implementation
  calculated = calculate_GCI(data.frame(timestamp = full_times, glucose = full_data_ma1),
                             weights = default_weights(),
                             means = c(long_slope = 0, long_midpoint = 0,
                                       int_slope = 0, int_midpoint = 0,
                                       short_slope = 0, short_midpoint = 0),
                             stddevs = c(long_slope = 15, long_midpoint = 1,
                                         int_slope = 15, int_midpoint = 1,
                                         short_slope = 15, short_midpoint = 1),
                             default_pgram())
  expect_equal(calculated, baseline)
})

test_that("GCI on DF matches previous implementation: MA(1)", {
  baseline = baseline_metrics %>%
    filter(ID == "full_ma") %>%
    select(GCI) %>%
    as.numeric() %>%
    unname()

  # Stddevs accommodate for frequency unit shift in old implementation
  calculated = calculate_GCI(data.frame(timestamp = full_times, glucose = full_data_ma1),
                             stddevs = c(long_slope = 15, long_midpoint = 1,
                                         int_slope = 15, int_midpoint = 1,
                                         short_slope = 15, short_midpoint = 1))
  expect_equal(calculated, baseline)
})

test_that("GCI on DF matches previous implementation: AR(1)", {
  baseline = baseline_metrics %>%
    filter(ID == "full_ar") %>%
    select(GCI) %>%
    as.numeric() %>%
    unname()

  # Stddevs accommodate for frequency unit shift in old implementation
  calculated = calculate_GCI(data.frame(timestamp = full_times, glucose = full_data_ar1),
                             stddevs = c(long_slope = 15, long_midpoint = 1,
                                         int_slope = 15, int_midpoint = 1,
                                         short_slope = 15, short_midpoint = 1))
  expect_equal(calculated, baseline)
})

test_that("GCI matches previous implementation: MA(1)", {
  baseline = baseline_metrics %>%
    filter(ID == "full_ma") %>%
    select(GCI) %>%
    as.numeric() %>%
    unname()

  fit = pgram(full_data_ma1, full_times)
  metrics = linear_approx(fit)
  # Accommodates for frequency unit shift in old implementation
  metrics$long_slope = metrics$long_slope/15
  metrics$int_slope = metrics$int_slope/15
  metrics$short_slope = metrics$short_slope/15
  calculated = calculate_GCI(metrics)

  expect_equal(calculated, baseline)
})

test_that("GCI matches previous implementation: AR(1)", {
  baseline = baseline_metrics %>%
    filter(ID == "full_ar") %>%
    select(GCI) %>%
    as.numeric() %>%
    unname()

  fit = pgram(full_data_ar1, full_times)
  metrics = linear_approx(fit)
  # Accommodates for frequency unit shift in old implementation
  metrics$long_slope = metrics$long_slope/15
  metrics$int_slope = metrics$int_slope/15
  metrics$short_slope = metrics$short_slope/15
  calculated = calculate_GCI(metrics)

  expect_equal(calculated, baseline)
})

test_that("GCI throws error without sufficient standardization mean elements", {
  fit = pgram(full_data_ar1, full_times)
  metrics = linear_approx(fit)
  # Accommodates for frequency unit shift in old implementation
  metrics$long_slope = metrics$long_slope/15
  metrics$int_slope = metrics$int_slope/15
  metrics$short_slope = metrics$short_slope/15
  expect_error(calculate_GCI(metrics, means = c(long_slope = 0, long_midpoint = 0)))
})

test_that("GCI throws error without sufficient standardization SD elements", {
  fit = pgram(full_data_ar1, full_times)
  metrics = linear_approx(fit)
  # Accommodates for frequency unit shift in old implementation
  metrics$long_slope = metrics$long_slope/15
  metrics$int_slope = metrics$int_slope/15
  metrics$short_slope = metrics$short_slope/15
  expect_error(calculate_GCI(metrics, stddevs = c(long_slope = 0, long_midpoint = 0)))
})
