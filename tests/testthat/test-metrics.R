test_that("6DF representation matches previous implementation: AR(1)", {
  baseline = baseline_metrics %>%
    filter(ID == "full_ar") %>%
    select(-c(ID, Row, GCI)) %>%
    as.numeric()

  names(baseline) = colnames(baseline_metrics)[3:(ncol(baseline_metrics)-1)]

  fit = pgram(full_data_ar1, full_times)
  metrics = linear_approx(fit)

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
  metrics = linear_approx(fit)

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
  metrics = linear_approx(fit)
  # Accommodates for frequency unit shift in old implementation
  metrics[c(1,3,5)] = metrics[c(1,3,5)]/15
  calculated = calculate_GCI(metrics)

  expect_equal(calculated, baseline)
})

test_that("slope/midpoint function operates as expected", {
  set.seed(1234)
  freqs = seq(-1, 1, length.out = 100)
  slope = rnorm(1)
  test_df = data.frame(frequencies = freqs, spectra = slope*freqs + rnorm(100, sd = 1E-4))
  test_obj = slope_midpoint(test_df, 0)
  expect_equal(test_obj, list(Slope = slope, Midpoint = 0), tolerance = 1E-3)
})
