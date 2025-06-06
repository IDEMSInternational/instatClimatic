# test-fmt_trace.R

test_that("fmt_trace handles NA values correctly", {
  expect_equal(fmt_trace(NA), NA)
  expect_equal(fmt_trace(c(NA, 0.03, 1)), c(NA, "tr", "1.00"))
})

test_that("fmt_trace replaces 0.03 with 'tr'", {
  expect_equal(fmt_trace(0.03), "tr")
  expect_equal(fmt_trace(c(0.03, 0.03)), c("tr", "tr"))
})

test_that("fmt_trace formats other numbers with 2 decimal places", {
  expect_equal(fmt_trace(0), "0.00")
  expect_equal(fmt_trace(1.5), "1.50")
  expect_equal(fmt_trace(c(0.1, 2, 3.456)), c("0.100", "2.000", "3.456"))
})