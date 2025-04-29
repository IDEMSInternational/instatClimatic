# convert_to_dec_deg
# convert_yy_to_yyyy

test_that("convert_to_dec_deg works for valid coordinates and directions", {
  expect_equal(convert_to_dec_deg(45, 30, 30, "N"), 45.508333, tolerance = 1e-6)
  expect_equal(convert_to_dec_deg(122, 10, 0, "W"), -122.166667, tolerance = 1e-6)
  expect_equal(convert_to_dec_deg(0, 0, 0, "E"), 0)
  expect_equal(convert_to_dec_deg(12.5, dir = "S"), -12.5)
})

test_that("convert_to_dec_deg handles NA direction and values correctly", {
  expect_true(is.na(convert_to_dec_deg(10, dir = NA)))
})

test_that("convert_to_dec_deg throws errors for invalid directions or ranges", {
  expect_error(convert_to_dec_deg(10, dir = "Z"), "dir must only contain direction letters")
  expect_error(convert_to_dec_deg(-10, dir = "N"), "dd must be positive if dir is supplied")
  expect_error(convert_to_dec_deg(10, mm = 70, dir = "E"), "mm must be between 0 and 60")
  expect_error(convert_to_dec_deg(10, ss = 70, dir = "E"), "ss must be between 0 and 60")
  expect_error(convert_to_dec_deg(), "dd must be supplied")
})

test_that("convert_yy_to_yyyy correctly converts years based on base", {
  expect_equal(convert_yy_to_yyyy(c(92, 98, 4, 88), base = 1990), c(1992, 1998, 1904, 1988))
  expect_equal(convert_yy_to_yyyy(0:99, base = 2000)[1:5], c(2000, 1901, 1902, 1903, 1904))  # lower end
  expect_equal(convert_yy_to_yyyy(99, base = 1999), 1999)
})

test_that("convert_yy_to_yyyy errors when base is missing", {
  expect_error(convert_yy_to_yyyy(98), "base year must be supplied")
})

test_that("spells handles typical sequences", {
  expect_equal(spells(c(1, 1, 1, 0, 1, 1)), c(NA, NA, NA, 0, 1, 2))
})

test_that("spells resets after zero", {
  expect_equal(spells(c(1, 0, 1)), c(NA, 0, 1))
})

test_that("spells works with custom initial value", {
  expect_equal(spells(c(1, 1, 0, 1), initial_value = 10), c(11, 12, 0, 1))
})

test_that("spells handles all zeros", {
  expect_equal(spells(c(0, 0, 0)), c(0, 0, 0))
})

test_that("spells handles leading zeros", {
  expect_equal(spells(c(0, 1, 1)), c(0, 1, 2))
})

test_that("spells handles empty vector", {
  expect_equal(spells(numeric(0)), numeric(0))
})

test_that("spells works with NA as initial value", {
  expect_equal(spells(c(1, 1, 0, 1), initial_value = NA_real_), c(NA_real_, NA_real_ + 1, 0, 1))
})
