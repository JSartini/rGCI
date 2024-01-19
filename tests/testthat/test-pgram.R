test_that("log-periodogram matches previous implementation: AR(1)", {
  baseline = baseline_pgram %>%
    filter(ID == "full_ar") %>%
    pull(Pgram)

  expect_equal(baseline, pgram(full_data_ar1, full_times)[["pgram"]])
})

test_that("log-periodogram matches previous implementation: MA(1)", {
  baseline = baseline_pgram %>%
    filter(ID == "full_ma") %>%
    pull(Pgram)

  expect_equal(baseline, pgram(full_data_ma1, full_times)[["pgram"]])
})

test_that("gaps in data throw error: AR(1)", {
  expect_error(pgram(gap_data_ar1, gap_times))
})

test_that("gaps in data throw error: MA(1)", {
  expect_error(pgram(gap_data_ma1, gap_times))
})

test_that("insufficient data throws error: AR(1)", {
  expect_error(pgram(ins_data_ar1, ins_times))
})

test_that("insufficient in data throw error: MA(1)", {
  expect_error(pgram(ins_data_ma1, ins_times))
})
