context("dd_to_dms")

test_that("dd_to_dms formats latitude and longitude correctly", {
  expect_equal(dd_to_dms(37.7749, TRUE), "37 46 30 N")
  expect_equal(dd_to_dms(-122.4194, FALSE), "122 25 10 W")
  expect_equal(dd_to_dms(0, TRUE), "00 00 00 N")
  expect_equal(dd_to_dms(0, FALSE), "000 00 00 E")
})
