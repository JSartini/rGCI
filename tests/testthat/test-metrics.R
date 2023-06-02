test_that("6DF representation matches previous implementation: AR(1)", {
  baseline = baseline_metrics %>%
    filter(ID == "full_ar") %>%
    select(-c(ID, Row)) %>%
    as.numeric()

  names(baseline) = colnames(baseline_metrics)[3:ncol(baseline_metrics)]

  fit = pgram(full_data_ar1, full_times)
  metrics = linear_approx(fit[["pgram"]], fit[["freqs"]])

  # Accommodates for frequency unit shift in old implementation
  metrics[c(1,3,5)] = metrics[c(1,3,5)]/15

  expect_equal(metrics, baseline)
})

test_that("6DF representation matches previous implementation: MA(1)", {
  baseline = baseline_metrics %>%
    filter(ID == "full_ma") %>%
    select(-c(ID, Row)) %>%
    as.numeric()

  names(baseline) = colnames(baseline_metrics)[3:ncol(baseline_metrics)]

  fit = pgram(full_data_ma1, full_times)
  metrics = linear_approx(fit[["pgram"]], fit[["freqs"]])

  # Accommodates for frequency unit shift in old implementation
  metrics[c(1,3,5)] = metrics[c(1,3,5)]/15

  expect_equal(metrics, baseline)
})
