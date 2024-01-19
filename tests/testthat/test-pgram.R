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

test_that("gaps in data throw error from pad_for_pgram: AR(1)", {
  expect_error(pgram(gap_data_ar1, gap_times))
})

test_that("gaps in data throw error from pad_for_pgram: MA(1)", {
  expect_error(pgram(gap_data_ma1, gap_times))
})

test_that("insufficient data throws error from pad_for_pgram: AR(1)", {
  expect_error(pgram(ins_data_ar1, ins_times))
})

test_that("insufficient data throws error from pad_for_pgram: MA(1)", {
  expect_error(pgram(ins_data_ma1, ins_times))
})

test_that("calculated constants are correct in standard cases", {
  test_output = c(calc_constants(15, 14),
                  calc_constants(5, 14),
                  calc_constants(5, 7))
  expected_output = list(per_day = 96, per_wear = 1344,
                         per_day = 288, per_wear = 4032,
                         per_day = 288, per_wear = 2016)
  expect_equal(test_output, expected_output)
})
