library(PCICt)
test_that("climdexInput.raw constructs a valid object", {
  # Simulated daily dates
  dates <- seq(as.Date("1981-01-01"), as.Date("1982-12-31"), by = "day")
  date_chars <- format(dates, "%Y-%m-%d")
  dates.pcict <- PCICt::as.PCICt(date_chars, format = "%Y-%m-%d", cal = "gregorian")
  
  # Simulate data with some NAs
  set.seed(42)
  tmax <- 25 + rnorm(length(dates))
  tmin <- 15 + rnorm(length(dates))
  prec <- runif(length(dates), 0, 10)
  tmax[seq(1, length(tmax), 50)] <- NA
  tmin[seq(1, length(tmin), 50)] <- NA
  prec[seq(1, length(prec), 50)] <- NA
  
  # Create the object
  ci <- climdexInput.raw(
    tmax = tmax, tmin = tmin, prec = prec,
    tmax.dates = dates.pcict,
    tmin.dates = dates.pcict,
    prec.dates = dates.pcict,
    base.range = c(1981, 1990),
    northern.hemisphere = TRUE
  )
  
  expect_s4_class(ci, "climdexInput")
  expect_true(validObject(ci))
  expect_equal(length(ci@dates), length(tmax))
  expect_equal(names(ci@data), c("tmax", "tmin", "prec", "tavg"))
})

test_that("quantiles are computed and stored in correct structure", {
  # Create as before
  dates <- seq(as.Date("1981-01-01"), as.Date("1982-12-31"), by = "day")
  date_chars <- format(dates, "%Y-%m-%d")
  dates.pcict <- PCICt::as.PCICt(date_chars, format = "%Y-%m-%d", cal = "gregorian")
  
  tmax <- 25 + rnorm(length(dates))
  tmin <- 15 + rnorm(length(dates))
  prec <- runif(length(dates), 0, 10)
  tmax[seq(1, length(tmax), 50)] <- NA
  
  ci <- climdexInput.raw(
    tmax = tmax, tmin = tmin, prec = prec,
    tmax.dates = dates.pcict,
    tmin.dates = dates.pcict,
    prec.dates = dates.pcict
  )
  
  expect_true("tmax" %in% ls(ci@quantiles))
  expect_true("q10" %in% names(ci@quantiles$tmax$outbase))
  expect_true("q90" %in% names(ci@quantiles$tmax$outbase))
})

test_that("climdexInput.raw uses provided quantiles when supplied", {
  # Create as before
  dates <- seq(as.Date("1981-01-01"), as.Date("1982-12-31"), by = "day")
  date_chars <- format(dates, "%Y-%m-%d")
  dates.pcict <- PCICt::as.PCICt(date_chars, format = "%Y-%m-%d", cal = "gregorian")
  
  prec <- runif(length(dates), 0, 10)
  
  q_env <- list(
    prec = list(
      q95 = 8,
      q99 = 9.5
    )
  )
  
  ci <- climdexInput.raw(
    prec = prec,
    prec.dates = dates.pcict,
    base.range = c(1981, 1983),
    quantiles = q_env
  )
  
  expect_s4_class(ci, "climdexInput")
  expect_equal(ci@quantiles$prec$q95, 8)
})

test_that("invalid quantile input throws error", {
  # Create as before
  dates <- seq(as.Date("1981-01-01"), as.Date("1982-12-31"), by = "day")
  date_chars <- format(dates, "%Y-%m-%d")
  dates.pcict <- PCICt::as.PCICt(date_chars, format = "%Y-%m-%d", cal = "gregorian")
  prec <- runif(length(dates))
  
  q_invalid <- list()  # no 'prec'
  
  expect_error(
    climdexInput.raw(
      prec = prec,
      prec.dates = dates.pcict,
      base.range = c(1981, 1982),
      quantiles = q_invalid
    ),
    "Quantiles must be present for all variables"
  )
})

make_test_climdex_input <- function() {
  # Create as before
  dates <- seq(as.Date("1981-01-01"), as.Date("1982-12-31"), by = "day")
  date_chars <- format(dates, "%Y-%m-%d")
  pcict_dates <- PCICt::as.PCICt(date_chars, format = "%Y-%m-%d", cal = "gregorian")
  tmax <- 25 + rnorm(length(dates))
  tmin <- 15 + rnorm(length(dates))
  prec <- runif(length(dates), 0, 10)
  
  climdexInput.raw(
    tmax = tmax,
    tmin = tmin,
    prec = prec,
    tmax.dates = pcict_dates,
    tmin.dates = pcict_dates,
    prec.dates = pcict_dates,
    base.range = c(1981, 1982)
  )
}

test_that("namasks and date.factors are the correct length and type", {
  ci <- make_test_climdex_input()  # your own helper (see below)
  
  expect_type(ci@namasks, "list")
  expect_type(ci@date.factors, "list")
  
  expect_true(all(c("annual", "monthly", "seasonal") %in% names(ci@date.factors)))
  expect_equal(length(ci@namasks$annual$tmax), length(levels(ci@date.factors$annual)))
})


test_that("valid.climdexInput returns TRUE for valid input", {
  # Create as before
  dates <- seq(as.Date("1981-01-01"), as.Date("1982-12-31"), by = "day")
  date_chars <- format(dates, "%Y-%m-%d")
  dates <- PCICt::as.PCICt(date_chars, format = "%Y-%m-%d", cal = "gregorian")
  jdays <- as.numeric(format(dates, "%j"))
  data <- list(
    tmax = rnorm(length(dates)),
    tmin = rnorm(length(dates)),
    prec = rnorm(length(dates))
  )
  date.factors <- list(
    annual = factor(format(dates, "%Y")),
    monthly = factor(format(dates, "%Y-%m")),
    seasonal = factor(rep(c("Winter 1980", "Spring 1980"), length.out = length(dates)))
  )
  namasks <- list(
    annual = list(tmax = rep(1, 2), tmin = rep(1, 2), prec = rep(1, 2)),
    monthly = list(tmax = rep(1, 24), tmin = rep(1, 24), prec = rep(1, 24)),
    seasonal = list(tmax = rep(1, 8), tmin = rep(1, 8), prec = rep(1, 8))
  )
  base.range <- as.PCICt(c("1980-01-01", "1990-12-31"), cal = "gregorian")
  
  ci <- new("climdexInput",
            data = data,
            quantiles = new.env(),
            namasks = namasks,
            dates = dates,
            jdays = jdays,
            base.range = base.range,
            date.factors = date.factors,
            northern.hemisphere = TRUE,
            max.missing.days = c(annual = 15, monthly = 3, seasonal = 6)
  )
  
  expect_true(validObject(ci))
})

test_that("valid.climdexInput errors on mismatched lengths", {
  dates <- seq(as.Date("1981-01-01"), as.Date("1982-12-31"), by = "day")
  date_chars <- format(dates, "%Y-%m-%d")
  dates <- PCICt::as.PCICt(date_chars, format = "%Y-%m-%d", cal = "gregorian")
  jdays <- 1:90  # mismatch!
  data <- list(tmax = rnorm(100))
  df <- list(annual = factor(rep("1980", 100)))
  namasks <- list(annual = list(tmax = rep(1, 1)),
                  monthly = list(tmax = rep(1, 1)),
                  seasonal = list(tmax = rep(1, 1)))
  base.range <- as.PCICt(c("1980-01-01", "1990-12-31"), cal = "gregorian")
  
  expect_error(new("climdexInput",
            data = data,
            quantiles = new.env(),
            namasks = namasks,
            dates = dates,
            jdays = jdays,
            base.range = base.range,
            date.factors = df,
            northern.hemisphere = TRUE,
            max.missing.days = c(annual = 15, monthly = 3, seasonal = 6)
  ), "Data fields, dates, and date factors must all be of the same length")
})

test_that("valid.climdexInput errors if namasks are incomplete", {
  dates <- seq(as.Date("1981-01-01"), as.Date("1982-12-31"), by = "day")
  date_chars <- format(dates, "%Y-%m-%d")
  dates <- PCICt::as.PCICt(date_chars, format = "%Y-%m-%d", cal = "gregorian")
  jdays <- 1:10
  data <- list(tmax = rnorm(10))
  df <- list(annual = factor(rep("1980", 10)))
  base.range <- as.PCICt(c("1980-01-01", "1990-12-31"), cal = "gregorian")
  
  namasks <- list(annual = list(), monthly = list(), seasonal = list())
  
  expect_error(new("climdexInput",
            data = data,
            quantiles = new.env(),
            namasks = namasks,
            dates = dates,
            jdays = jdays,
            base.range = base.range,
            date.factors = df,
            northern.hemisphere = TRUE,
            max.missing.days = c(annual = 15, monthly = 3, seasonal = 6)), "NA mask for monthly, seasonal and annual must contain data for all variables supplied")
})

test_that("valid.climdexInput errors if northern.hemisphere is not length 1", {
  dates <- seq(as.Date("1981-01-01"), as.Date("1982-12-31"), by = "day")
  date_chars <- format(dates, "%Y-%m-%d")
  dates <- PCICt::as.PCICt(date_chars, format = "%Y-%m-%d", cal = "gregorian")
  jdays <- 1:10
  data <- list(tmax = rnorm(10))
  df <- list(annual = factor(rep("1980", 10)))
  base.range <- as.PCICt(c("1980-01-01", "1990-12-31"), cal = "gregorian")
  
  namasks <- list(
    annual = list(tmax = rep(1, 1)),
    monthly = list(tmax = rep(1, 1)),
    seasonal = list(tmax = rep(1, 1))
  )
  
  expect_error(new("climdexInput",
            data = data,
            quantiles = new.env(),
            namasks = namasks,
            dates = dates,
            jdays = jdays,
            base.range = base.range,
            date.factors = df,
            northern.hemisphere = c(TRUE, FALSE),
            max.missing.days = c(annual = 15, monthly = 3, seasonal = 6)), "northern.hemisphere must be of length 1")
})



#######################################################################################################################
#usethis::use_data_raw("synthetic_climate", open = FALSE)

test_that("climdex output snapshot is stable", {
  data <- read.csv(testthat::test_path("test-data", "synthetic_climate.csv"))
  indices <- c("fd", "su", "r10mm", "sdii", "gsl")
  
  out <- climdex(
    data = data,
    station = "station",
    date = "date",
    year = "year",
    month = "month",
    prec = "precip",
    tmax = "tmax",
    tmin = "tmin",
    indices = indices
  )
  
  expect_equal(class(out), "data.frame")
})

test_that("indices return plausible values", {
  data <- read.csv(testthat::test_path("test-data", "synthetic_climate.csv"))
  indices <- c("fd", "r10mm", "gsl", "cdd")
  
  out <- climdex(
    data = data,
    station = "station",
    date = "date",
    year = "year",
    month = "month",
    prec = "precip",
    tmax = "tmax",
    tmin = "tmin",
    indices = indices
  )
  
  expect_true(all(out$fd >= 0, na.rm = TRUE))
  expect_true(all(out$r10mm >= 0, na.rm = TRUE))
  expect_true(all(out$gsl >= 0, na.rm = TRUE))
  expect_true(all(out$cdd >= 0, na.rm = TRUE))
})

test_that("climdex works with southern hemisphere and different base range", {
  data <- read.csv(testthat::test_path("test-data", "synthetic_climate.csv"))
  indices <- c("gsl")
  
  out <- climdex(
    data = data,
    station = "station",
    date = "date",
    year = "year",
    month = "month",
    prec = "precip",
    tmax = "tmax",
    tmin = "tmin",
    indices = indices,
    northern.hemisphere = FALSE,
    base.range = c(1981, 1982)
  )
  
  expect_true("gsl" %in% names(out))
  expect_true(all(out$gsl >= 0, na.rm = TRUE))
})

test_that("climdex handles all-NA rows", {
  data <- read.csv(testthat::test_path("test-data", "synthetic_climate.csv"))
  data$tmax[1:10] <- NA
  data$tmin[1:10] <- NA
  data$precip[1:10] <- NA
  
  indices <- c("fd", "cdd")
  
  out <- climdex(
    data = data,
    station = "station",
    date = "date",
    year = "year",
    month = "month",
    prec = "precip",
    tmax = "tmax",
    tmin = "tmin",
    indices = indices
  )
  
  expect_s3_class(out, "data.frame")
  expect_true("cdd" %in% names(out))
})

test_that("climdex computes indices for a single station", {
  data <- read.csv(testthat::test_path("test-data", "synthetic_climate.csv"))
  indices <- c("fd", "su", "r10mm", "sdii", "gsl")
  
  out <- climdex(
    data = data,
    station = "station",
    date = "date",
    year = "year",
    month = "month",
    prec = "precip",
    tmax = "tmax",
    tmin = "tmin",
    indices = indices
  )
  
  expect_s3_class(out, "data.frame")
  expect_true(all(c("year", "fd", "su", "r10mm", "sdii", "gsl") %in% colnames(out)))
})

test_that("climdex fails when monthly frequency is selected without month", {
  data <- read.csv(testthat::test_path("test-data", "synthetic_climate.csv"))
  indices <- c("sdii")
  
  expect_error(
    climdex(
      data = data,
      station = "station",
      date = "date",
      year = "year",
      prec = "precip",
      tmax = "tmax",
      tmin = "tmin",
      indices = indices,
      freq = "monthly"
    ),
    "month is required for freq = 'monthly'"
  )
})

test_that("climdex fails when year-only indices used with monthly freq", {
  data <- read.csv(testthat::test_path("test-data", "synthetic_climate.csv"))
  indices <- c("fd", "tr")
  
  expect_error(
    climdex(
      data = data,
      station = "station",
      date = "date",
      year = "year",
      month = "month",
      prec = "precip",
      tmax = "tmax",
      tmin = "tmin",
      indices = indices,
      freq = "monthly"
    ),
    "Some indices selected are not available on a monthly frequency"
  )
})

test_that("climdex can handle multiple stations", {
  data <- read.csv(testthat::test_path("test-data", "synthetic_climate.csv"))
  data$station <- rep(c("S1", "S2"), length.out = nrow(data))
  indices <- c("fd", "su")
  
  out <- climdex(
    data = data,
    station = "station",
    date = "date",
    year = "year",
    month = "month",
    prec = "precip",
    tmax = "tmax",
    tmin = "tmin",
    indices = indices
  )
  
  expect_s3_class(out, "data.frame")
  expect_true(all(c("station", "year", "fd", "su") %in% names(out)))
  expect_true(all(unique(out$station) %in% c("S1", "S2")))
})

test_that("climdex handles gsl mode and spell threshold", {
  data <- read.csv(testthat::test_path("test-data", "synthetic_climate.csv"))
  indices <- c("gsl", "cdd", "cwd")
  
  out <- climdex(
    data = data,
    station = "station",
    date = "date",
    year = "year",
    month = "month",
    prec = "precip",
    tmax = "tmax",
    tmin = "tmin",
    indices = indices,
    gsl.mode = "GSL",
    threshold = 5
  )
  
  expect_true("gsl" %in% names(out))
  expect_true("cdd" %in% names(out))
  expect_true("cwd" %in% names(out))
})


