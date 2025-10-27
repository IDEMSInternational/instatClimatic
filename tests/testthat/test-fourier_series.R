context("fourier_series")

test_that("fourier_series builds expected string for small n", {
  out <- fourier_series(10, 2, 5)
  expected <- paste(
    "sin(10 * 1 * 2 * pi / 5) + cos(10 * 1 * 2 * pi / 5)",
    "sin(10 * 2 * 2 * pi / 5) + cos(10 * 2 * 2 * pi / 5)",
    sep = " + "
  )
  expect_equal(out, expected)
})
