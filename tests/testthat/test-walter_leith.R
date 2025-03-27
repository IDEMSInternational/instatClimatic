library(testthat)
library(dplyr)
library(tibble)
library(forcats)

test_that("cumulative_inventory works without station", {
  df <- tibble(
    start_date = rep(1:2, each = 3),
    end_date = rep(1:3, 2)
  )
  result <- cumulative_inventory(df, from = "start_date", to = "end_date")
  expect_true(all(c("cum", "cum1") %in% names(result)))
  expect_equal(nrow(result), 6)
  expect_type(result$cum1, "integer")
})

test_that("cumulative_inventory works with station", {
  df <- tibble(
    station = rep(c("A", "B"), each = 3),
    start_date = rep(1:2, each = 3),
    end_date = rep(1:3, 2)
  )
  result <- cumulative_inventory(df, station = "station", from = "start_date", to = "end_date")
  expect_true(all(c("cum", "cum1") %in% names(result)))
  expect_equal(length(unique(result$station)), 2)
})

test_that("WB_evaporation handles full saturation", {
  expect_equal(WB_evaporation(200, 0.5, 200, 10, 5), 10)
})

test_that("WB_evaporation handles dry conditions without rain", {
  evap <- WB_evaporation(40, 0.5, 200, 10, 0)
  expect_lt(evap, 10)
})

test_that("WB_evaporation handles dry conditions with some rain", {
  evap <- WB_evaporation(40, 0.5, 200, 10, 5)
  expect_gt(evap, 0)
  expect_lt(evap, 10)
})

test_that("write_weather_data writes expected format", {
  tmp <- tempfile(fileext = ".txt")
  write_weather_data(
    year = c(2020, 2020),
    month = c(1, 1),
    day = c(1, 2),
    rain = c(1.0, NA),
    mn_tmp = c(10, 12),
    mx_tmp = c(20, 22),
    missing_code = -99,
    output_file = tmp
  )
  expect_true(file.exists(tmp))
  out <- read.table(tmp, header = TRUE)
  expect_equal(out$rain[2], -99)
  unlink(tmp)
})

test_that("ggwalter_lieth returns ggplot object", {
  df <- tibble(
    Month = forcats::fct_inorder(month.abb),
    Precipitation = c(20, 30, 40, 60, 70, 90, 100, 80, 50, 30, 25, 15),
    MaxTemp = seq(5, 28, length.out = 12),
    MinTemp = seq(-2, 15, length.out = 12),
    AvgTemp = seq(1, 20, length.out = 12)
  )
  plt <- suppressWarnings(ggwalter_lieth(df, month = "Month", p_mes = "Precipitation",
                        tm_max = "MaxTemp", tm_min = "MinTemp", ta_min = "AvgTemp"))
  expect_s3_class(plt, "ggplot")
})

# test_that("prepare_walter_lieth returns expected structure", {
#   df <- tibble(
#     indrow = 0:12,
#     Month = rep("", 13),  # skip string column
#     tm = seq(5, 17, length.out = 13),
#     pm_reesc = seq(10, 90, length.out = 13),
#     MinTemp = seq(0, 12),
#     AvgTemp = seq(3, 15)
#   )
#   result <- prepare_walter_lieth(df, month = "Month", tm_min = "MinTemp", ta_min = "AvgTemp")
#   expect_type(result, "list")
#   expect_named(result, c("dat_long_end", "tm_max_line", "pm_max_line",
#                          "prep_max_poly", "prob_freeze", "surefreeze"))
# })
