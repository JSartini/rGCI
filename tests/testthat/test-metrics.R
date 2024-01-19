test_that("6DF representation matches previous implementation: AR(1)", {
  baseline = baseline_metrics %>%
    filter(ID == "full_ar") %>%
    select(-c(ID, Row, GCI)) %>%
    as.numeric()

  names(baseline) = colnames(baseline_metrics)[3:(ncol(baseline_metrics)-1)]

  fit = pgram(full_data_ar1, full_times)
  metrics = linear_approx(fit[["pgram"]], fit[["freqs"]])

  # Accommodates for frequency unit shift in old implementation
  metrics[c(1,3,5)] = metrics[c(1,3,5)]/15

  expect_equal(metrics, baseline)
})

test_that("6DF representation matches previous implementation: MA(1)", {
  baseline = baseline_metrics %>%
    filter(ID == "full_ma") %>%
    select(-c(ID, Row, GCI)) %>%
    as.numeric()

  names(baseline) = colnames(baseline_metrics)[3:(ncol(baseline_metrics)-1)]

  fit = pgram(full_data_ma1, full_times)
  metrics = linear_approx(fit[["pgram"]], fit[["freqs"]])

  # Accommodates for frequency unit shift in old implementation
  metrics[c(1,3,5)] = metrics[c(1,3,5)]/15

  expect_equal(metrics, baseline)
})

test_that("GCI matches previous implementation: AR(1)", {
  baseline = baseline_metrics %>%
    filter(ID == "full_ar") %>%
    select(GCI) %>%
    as.numeric() %>%
    unname()

  fit = pgram(full_data_ar1, full_times)
  metrics = linear_approx(fit[["pgram"]], fit[["freqs"]])
  # Accommodates for frequency unit shift in old implementation
  metrics[c(1,3,5)] = metrics[c(1,3,5)]/15
  calculated = calculate_GCI(metrics)

  expect_equal(calculated, baseline)
})

test_that("GCI matches previous implementation: MA(1)", {
  baseline = baseline_metrics %>%
    filter(ID == "full_ma") %>%
    select(GCI) %>%
    as.numeric() %>%
    unname()

  fit = pgram(full_data_ma1, full_times)
  metrics = linear_approx(fit[["pgram"]], fit[["freqs"]])
  # Accommodates for frequency unit shift in old implementation
  metrics[c(1,3,5)] = metrics[c(1,3,5)]/15
  calculated = calculate_GCI(metrics)

  expect_equal(calculated, baseline)
})

test_that("GCI throws error without sufficient standardization mean elements", {
  fit = pgram(full_data_ar1, full_times)
  metrics = linear_approx(fit[["pgram"]], fit[["freqs"]])
  # Accommodates for frequency unit shift in old implementation
  metrics[c(1,3,5)] = metrics[c(1,3,5)]/15
  expect_error(calculate_GCI(metrics, means = c(long_slope = 0, long_midpoint = 0)))
})

test_that("GCI throws error without sufficient standardization SD elements", {
  fit = pgram(full_data_ar1, full_times)
  metrics = linear_approx(fit[["pgram"]], fit[["freqs"]])
  # Accommodates for frequency unit shift in old implementation
  metrics[c(1,3,5)] = metrics[c(1,3,5)]/15
  expect_error(calculate_GCI(metrics, stddevs = c(long_slope = 0, long_midpoint = 0)))
})
