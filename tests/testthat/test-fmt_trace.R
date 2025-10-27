context("fmt_trace")

test_that("fmt_trace replaces trace value and preserves NA", {
  x <- c(0, 0.03, 1.5, NA)
  res <- fmt_trace(x)
  expect_equal(res[1], "0.00")
  expect_equal(res[2], "tr")
  expect_equal(res[3], "1.50")
  expect_true(is.na(res[4]))
})
