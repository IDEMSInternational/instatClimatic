library(PCICt)
data <- read.csv("testdata/synthetic_climate.csv")

test_that("climdex.pcic::climdexInput.raw constructs a valid object", {
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
  ci <- climdex.pcic::climdexInput.raw(
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

  ci <- climdex.pcic::climdexInput.raw(
    tmax = tmax, tmin = tmin, prec = prec,
    tmax.dates = dates.pcict,
    tmin.dates = dates.pcict,
    prec.dates = dates.pcict
  )

  expect_true("tmax" %in% ls(ci@quantiles))
  expect_true("q10" %in% names(ci@quantiles$tmax$outbase))
  expect_true("q90" %in% names(ci@quantiles$tmax$outbase))
})

test_that("climdex.pcic::climdexInput.raw uses provided quantiles when supplied", {
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

  ci <- climdex.pcic::climdexInput.raw(
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
    climdex.pcic::climdexInput.raw(
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

  climdex.pcic::climdexInput.raw(
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


# test_that("valid.climdexInput returns TRUE for valid input", {
#   # Create as before
#   dates <- seq(as.Date("1981-01-01"), as.Date("1982-12-31"), by = "day")
#   date_chars <- format(dates, "%Y-%m-%d")
#   dates <- PCICt::as.PCICt(date_chars, format = "%Y-%m-%d", cal = "gregorian")
#   jdays <- as.numeric(format(dates, "%j"))
#   data <- list(
#     tmax = rnorm(length(dates)),
#     tmin = rnorm(length(dates)),
#     prec = rnorm(length(dates))
#   )
#   date.factors <- list(
#     annual = factor(format(dates, "%Y")),
#     monthly = factor(format(dates, "%Y-%m")),
#     seasonal = factor(rep(c("Winter 1980", "Spring 1980"), length.out = length(dates)))
#   )
#   namasks <- list(
#     annual = list(tmax = rep(1, 2), tmin = rep(1, 2), prec = rep(1, 2)),
#     monthly = list(tmax = rep(1, 24), tmin = rep(1, 24), prec = rep(1, 24)),
#     seasonal = list(tmax = rep(1, 8), tmin = rep(1, 8), prec = rep(1, 8))
#   )
#   base.range <- as.PCICt(c("1980-01-01", "1990-12-31"), cal = "gregorian")
# 
#   ci <- new("climdexInput",
#             data = data,
#             quantiles = new.env(),
#             namasks = namasks,
#             dates = dates,
#             jdays = jdays,
#             base.range = base.range,
#             date.factors = date.factors,
#             northern.hemisphere = TRUE,
#             max.missing.days = c(annual = 15, monthly = 3, seasonal = 6)
#   )
# 
#   expect_true(validObject(ci))
# })

# test_that("valid.climdexInput errors on mismatched lengths", {
#   dates <- seq(as.Date("1981-01-01"), as.Date("1982-12-31"), by = "day")
#   date_chars <- format(dates, "%Y-%m-%d")
#   dates <- PCICt::as.PCICt(date_chars, format = "%Y-%m-%d", cal = "gregorian")
#   jdays <- 1:90  # mismatch!
#   data <- list(tmax = rnorm(100))
#   df <- list(annual = factor(rep("1980", 100)))
#   namasks <- list(annual = list(tmax = rep(1, 1)),
#                   monthly = list(tmax = rep(1, 1)),
#                   seasonal = list(tmax = rep(1, 1)))
#   base.range <- as.PCICt(c("1980-01-01", "1990-12-31"), cal = "gregorian")
#
#   expect_error(new("climdexInput",
#                    data = data,
#                    quantiles = new.env(),
#                    namasks = namasks,
#                    dates = dates,
#                    jdays = jdays,
#                    base.range = base.range,
#                    date.factors = df,
#                    northern.hemisphere = TRUE,
#                    max.missing.days = c(annual = 15, monthly = 3, seasonal = 6)
#   ), "Data fields, dates, and date factors must all be of the same length")
# })

# test_that("valid.climdexInput errors if namasks are incomplete", {
#   dates <- seq(as.Date("1981-01-01"), as.Date("1982-12-31"), by = "day")
#   date_chars <- format(dates, "%Y-%m-%d")
#   dates <- PCICt::as.PCICt(date_chars, format = "%Y-%m-%d", cal = "gregorian")
#   jdays <- 1:10
#   data <- list(tmax = rnorm(10))
#   df <- list(annual = factor(rep("1980", 10)))
#   base.range <- as.PCICt(c("1980-01-01", "1990-12-31"), cal = "gregorian")
#
#   namasks <- list(annual = list(), monthly = list(), seasonal = list())
#
#   expect_error(new("climdexInput",
#                    data = data,
#                    quantiles = new.env(),
#                    namasks = namasks,
#                    dates = dates,
#                    jdays = jdays,
#                    base.range = base.range,
#                    date.factors = df,
#                    northern.hemisphere = TRUE,
#                    max.missing.days = c(annual = 15, monthly = 3, seasonal = 6)), "NA mask for monthly, seasonal and annual must contain data for all variables supplied")
# })
#
# test_that("valid.climdexInput errors if northern.hemisphere is not length 1", {
#   dates <- seq(as.Date("1981-01-01"), as.Date("1982-12-31"), by = "day")
#   date_chars <- format(dates, "%Y-%m-%d")
#   dates <- PCICt::as.PCICt(date_chars, format = "%Y-%m-%d", cal = "gregorian")
#   jdays <- 1:10
#   data <- list(tmax = rnorm(10))
#   df <- list(annual = factor(rep("1980", 10)))
#   base.range <- as.PCICt(c("1980-01-01", "1990-12-31"), cal = "gregorian")
#
#   namasks <- list(
#     annual = list(tmax = rep(1, 1)),
#     monthly = list(tmax = rep(1, 1)),
#     seasonal = list(tmax = rep(1, 1))
#   )
#
#   expect_error(new("climdexInput",
#                    data = data,
#                    quantiles = new.env(),
#                    namasks = namasks,
#                    dates = dates,
#                    jdays = jdays,
#                    base.range = base.range,
#                    date.factors = df,
#                    northern.hemisphere = c(TRUE, FALSE),
#                    max.missing.days = c(annual = 15, monthly = 3, seasonal = 6)), "northern.hemisphere must be of length 1")
# })
#
#

#######################################################################################################################
#usethis::use_data_raw("synthetic_climate", open = FALSE)
data <- read.csv("testdata/synthetic_climate.csv")

test_that("climdex output snapshot is stable", {
  indices <- c("fd", "su", "r10mm", "sdii", "gsl")
  
  out <- suppressWarnings(climdex(
    data = data,
    station = "station",
    date = "date",
    year = "year",
    month = "month",
    prec = "precip",
    tmax = "tmax",
    tmin = "tmin",
    indices = indices
  ))
  
  expect_equal(class(out), c("data.frame"))
})

test_that("indices return plausible values", {
  indices <- c("fd", "r10mm", "gsl", "cdd")
  
  out <- (climdex(
    data = data,
    station = "station",
    date = "date",
    year = "year",
    month = "month",
    prec = "precip",
    tmax = "tmax",
    tmin = "tmin",
    indices = indices
  ))
  
  expect_true(all(out$fd >= 0, na.rm = TRUE))
  expect_true(all(out$r10mm >= 0, na.rm = TRUE))
  expect_true(all(out$gsl >= 0, na.rm = TRUE))
  expect_true(all(out$cdd >= 0, na.rm = TRUE))
})

test_that("climdex works with southern hemisphere and different base range", {
  indices <- c("gsl")
  
  out <- climdex(
    data = climdex_data,
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

# test_that("climdex fails when monthly frequency is selected without month", {
#   indices <- c("sdii")
#   
#   expect_error(
#     climdex(
#       data = data,
#       station = "station",
#       date = "date",
#       year = "year",
#       prec = "precip",
#       tmax = "tmax",
#       tmin = "tmin",
#       indices = indices,
#       freq = "monthly"
#     ),
#     "month is required for freq = 'monthly'"
#   )
# })

test_that("climdex fails when year-only indices used with monthly freq", {
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

################################################################################
# synthetic_climate <- data
# 
# test_that("climdex precipitation-based indices run correctly", {
#   
#   # Create PCICt dates
#   dates <- PCICt::as.PCICt(as.character(synthetic_climate$date), cal = "gregorian")
#   
#   # Create climdexInput object
#   ci <- climdex.pcic::climdexInput.raw(
#     tmax = synthetic_climate$tmax,
#     tmin = synthetic_climate$tmin,
#     prec = synthetic_climate$precip,
#     tmax.dates = dates,
#     tmin.dates = dates,
#     prec.dates = dates,
#     base.range = c(1981, 1990)
#   )
#   
#   # Basic checks for each function
#   expect_type(climdex.rx1day(ci), "double")
#   expect_type(climdex.rx5day(ci), "double")
#   expect_type(climdex.sdii(ci), "double")
#   expect_type(climdex.r10mm(ci), "double")
#   expect_type(climdex.r20mm(ci), "double")
#   expect_type(climdex.rnnmm(ci, 15), "double")
#   expect_error(climdex.rnnmm(ci, c(1, 2))) # Invalid threshold input
#   expect_type(climdex.cdd(ci), "double")
#   expect_type(climdex.cwd(ci), "double")
#   expect_type(climdex.r95ptot(ci), "double")
#   expect_type(climdex.r99ptot(ci), "double")
#   expect_type(climdex.prcptot(ci), "double")
#   
#   # Test with exact dates
#   
#   
#   
#   expect_s3_class(climdex.rx1day(ci, freq = "annual", include.exact.dates = TRUE), "data.frame")
#   expect_s3_class(climdex.rx5day(ci, freq = "annual", include.exact.dates = TRUE), "data.frame")
#   expect_s3_class(climdex.cdd(ci, include.exact.dates = TRUE), "data.frame")
#   expect_s3_class(climdex.cwd(ci, include.exact.dates = TRUE), "data.frame")
# })
# 
# dates <- as.PCICt(as.character(data$date), cal = "gregorian")
# ci <- climdex.pcic::climdexInput.raw(
#   tmin = data$tmin,
#   tmax = data$tmax,
#   tavg = data$tavg,
#   prec = data$precip,
#   tmin.dates = dates,
#   tmax.dates = dates,
#   tavg.dates = dates,
#   prec.dates = dates,
#   base.range = c(1981, 1982)
# )
# 
# test_that("climdex.fd (frost days) returns numeric vector of correct length", {
#   result <- climdex.fd(ci)
#   expect_type(result, "double")
#   expect_equal(length(result), length(unique(format(ci@dates, "%Y"))))
# })
# 
# test_that("climdex.su (summer days) returns numeric vector", {
#   result <- climdex.su(ci)
#   expect_type(result, "double")
# })
# 
# test_that("climdex.tr (tropical nights) returns expected result", {
#   result <- climdex.tr(ci)
#   expect_type(result, "double")
# })
# 
# test_that("climdex.id (icing days) returns expected result", {
#   result <- climdex.id(ci)
#   expect_type(result, "double")
# })
# 
# test_that("climdex.tx90p and climdex.tn10p return numeric vectors", {
#   tx <- climdex.tx90p(ci)
#   tn <- climdex.tn10p(ci)
#   expect_type(tx, "double")
#   expect_type(tn, "double")
# })
# 
# test_that("climdex.wsdi and climdex.csdi return numeric vectors", {
#   ws <- climdex.wsdi(ci)
#   cs <- climdex.csdi(ci)
#   expect_type(ws, "double")
#   expect_type(cs, "double")
# })
# 
# test_that("climdex.gsl returns numeric vector with correct length", {
#   gsl <- climdex.gsl(ci)
#   expect_type(gsl, "double")
#   expect_equal(length(gsl), length(unique(format(ci@dates, "%Y"))))
# })
# 
# test_that("climdex.gsl returns start/end dates when include.exact.dates = TRUE", {
#   gsl_df <- climdex.gsl(ci, include.exact.dates = TRUE)
#   expect_s3_class(gsl_df, "data.frame")
#   expect_true(all(c("start", "sl", "end") %in% names(gsl_df)))
# })
# 
# test_that("climdex.dtr returns expected vector", {
#   dtr <- climdex.dtr(ci)
#   expect_type(dtr, "double")
#   expect_equal(length(dtr), 36)
# })

################################################################################
synthetic_climate <- data

test_that("climdex_single_station computes all indices with expected structure", {
  dates_pcic <- PCICt::as.PCICt(as.character(synthetic_climate$date), cal = "gregorian")
  ci <- climdex.pcic::climdexInput.raw(
    tmax = synthetic_climate$tmax, tmin = synthetic_climate$tmin, prec = synthetic_climate$precip,
    tmax.dates = dates_pcic, tmin.dates = dates_pcic, prec.dates = dates_pcic,
    base.range = c(1981, 1990), northern.hemisphere = TRUE
  )
  
  all_indices <- c(
    "fd", "su", "id", "tr", "wsdi", "csdi", "gsl", "txx", "txn", "tnx", "tnn",
    "tn10p", "tx10p", "tn90p", "tx90p", "dtr", "rx1day", "rx5day", "sdii",
    "r10mm", "r20mm", "rnnmm", "cdd", "cwd", "r95ptot", "r99ptot", "prcptot"
  )
  
  # Annual frequency
  df_annual <- suppressWarnings(climdex_single_station(ci = ci, freq = "annual", indices = all_indices, year = "year"))
  expect_s3_class(df_annual, "data.frame")
  expect_true(all(c("year", all_indices[!all_indices %in% "rnnmm"]) %in% names(df_annual)))
  expect_true(any(grepl("rnnmm_", names(df_annual))))
  
  # Monthly frequency subset
  df_monthly <- suppressWarnings(climdex_single_station(ci = ci, freq = "monthly", indices = c("txx", "tn10p", "dtr"), year = "year", month = "month"))
  expect_s3_class(df_monthly, "data.frame")
  expect_true(all(c("year", "month", "txx", "tn10p", "dtr") %in% names(df_monthly)))
})

test_that("climdex_single_station throws errors on bad input", {
  ci <- climdex.pcic::climdexInput.raw(
    tmax = synthetic_climate$tmax, tmin = synthetic_climate$tmin, prec = synthetic_climate$precip,
    tmax.dates = PCICt::as.PCICt(as.character(synthetic_climate$date), cal = "gregorian"),
    tmin.dates = PCICt::as.PCICt(as.character(synthetic_climate$date), cal = "gregorian"),
    prec.dates = PCICt::as.PCICt(as.character(synthetic_climate$date), cal = "gregorian"),
    base.range = c(1981, 1990), northern.hemisphere = TRUE
  )
  
  expect_error(climdex_single_station(ci = ci, indices = "fd", year = "year", freq = "monthly"), "month is required")
  expect_error(climdex_single_station(ci = ci, freq = "annual", year = "year"), "No indices specified")
  expect_error(climdex_single_station(ci = ci, indices = "invalid", year = "year"), "not recognised")
})

test_that("climdex_single_station computes temperature-based indices correctly", {
  # Use a wide date range to support quantile-based indices
  dates <- seq(as.Date("1980-01-01"), as.Date("1990-12-31"), by = "day")
  n <- length(dates)
  
  df <- data.frame(
    date = dates,
    year = as.numeric(format(dates, "%Y")),
    month = as.numeric(format(dates, "%m")),
    tmax = rnorm(n, 30, 5),
    tmin = rnorm(n, 20, 5),
    prec = rexp(n, 1 / 5)
  )
  df$tavg <- (df$tmax + df$tmin) / 2
  
  ci <- climdex.pcic::climdexInput.raw(
    tmin = df$tmin,
    tmax = df$tmax,
    prec = df$prec,
    tmin.dates = PCICt::as.PCICt(as.character(df$date), cal = "gregorian"),
    tmax.dates = PCICt::as.PCICt(as.character(df$date), cal = "gregorian"),
    prec.dates = PCICt::as.PCICt(as.character(df$date), cal = "gregorian"),
    base.range = c(1981, 1990),
    temp.qtiles = c(0.1, 0.9)
    )
  
  out <- climdex_single_station(
    ci = ci,
    freq = "annual",
    indices = c("fd", "su", "tn10p", "tx90p", "wsdi", "csdi", "gsl", "dtr"),
    year = "year"
  )
  
  expect_s3_class(out, "data.frame")
  expect_named(out, c("year", "fd", "su", "tn10p", "tx90p", "wsdi", "csdi", "gsl", "dtr"))
  expect_equal(nrow(out), length(unique(df$year)))
  expect_true(all(sapply(out[-1], is.numeric)))
  expect_true(all(out$year %in% df$year))
})

test_that("climdex fallback block without station works with various year/month formats", {
  dates <- seq(as.Date("1981-01-01"), as.Date("1981-12-31"), by = "day")
  n <- length(dates)
  
  #for (year_col in c("year_num", "year_factor", "year_char")) {
  #  for (month_col in c("month_num", "month_factor", "month_char")) {
  
  year_col <- "year_num"
  month_col <- "month_num"
  df <- data.frame(
    date = dates,
    year_num = as.numeric(format(dates, "%Y")),
    year_factor = factor(format(dates, "%Y")),
    year_char = as.character(format(dates, "%Y")),
    month_num = as.numeric(format(dates, "%m")),
    month_factor = factor(format(dates, "%m"), levels = sprintf("%02d", 1:12), ordered = TRUE),
    month_char = month.abb[as.numeric(format(dates, "%m"))],
    tmax = 25 + 5 * sin(2 * pi * (1:n) / 365),
    tmin = 15 + 5 * sin(2 * pi * (1:n) / 365),
    precip = rgamma(n, shape = 2, scale = 1.5)
  )
  
  expect_error(climdex(
    data = df,
    date = "date",
    year = year_col,
    month = month_col,
    prec = "precip",
    tmax = "tmax",
    tmin = "tmin",
    indices = c("sdii"),
    freq = "monthly"))
  
  ci_1 <- climdex(
    data = df,
    date = "date",
    year = year_col,
    month = month_col,
    prec = "precip",
    tmax = "tmax",
    tmin = "tmin",
    indices = c("rx1day"),
    freq = "monthly")
  
  expect_s3_class(ci_1, "data.frame")
  expect_true("rx1day" %in% names(ci_1))
  expect_equal(length(unique(ci_1[[year_col]])), 1)
  expect_equal(length(unique(ci_1[[month_col]])), 12)
  
  ###
  year_col <- "year_factor"
  month_col <- "month_factor"
  df <- data.frame(
    date = dates,
    year_num = as.numeric(format(dates, "%Y")),
    year_factor = factor(format(dates, "%Y")),
    year_char = as.character(format(dates, "%Y")),
    month_num = as.numeric(format(dates, "%m")),
    month_factor = factor(format(dates, "%m"), levels = sprintf("%02d", 1:12), ordered = TRUE),
    month_char = month.abb[as.numeric(format(dates, "%m"))],
    tmax = 25 + 5 * sin(2 * pi * (1:n) / 365),
    tmin = 15 + 5 * sin(2 * pi * (1:n) / 365),
    precip = rgamma(n, shape = 2, scale = 1.5)
  )
  
  ci_2 <- climdex(
    data = df,
    date = "date",
    year = year_col,
    month = month_col,
    prec = "precip",
    tmax = "tmax",
    tmin = "tmin",
    indices = c("rx1day"),
    freq = "monthly")
  expect_s3_class(ci_2, "data.frame")
  expect_true("rx1day" %in% names(ci_2))
  expect_equal(length(unique(ci_2[[year_col]])), 1)
  expect_equal(length(unique(ci_2[[month_col]])), 12)
  
  ###
  year_col <- "year_char"
  month_col <- "month_char"
  df <- data.frame(
    date = dates,
    year_num = as.numeric(format(dates, "%Y")),
    year_factor = factor(format(dates, "%Y")),
    year_char = as.character(format(dates, "%Y")),
    month_num = as.numeric(format(dates, "%m")),
    month_factor = factor(format(dates, "%m"), levels = sprintf("%02d", 1:12), ordered = TRUE),
    month_char = month.abb[as.numeric(format(dates, "%m"))],
    tmax = 25 + 5 * sin(2 * pi * (1:n) / 365),
    tmin = 15 + 5 * sin(2 * pi * (1:n) / 365),
    precip = rgamma(n, shape = 2, scale = 1.5)
  )
  
  # expect_error(suppressWarnings(climdex(
  #   data = df,
  #   date = "date",
  #   year = year_col,
  #   month = month_col,
  #   prec = "precip",
  #   tmax = "tmax",
  #   tmin = "tmin",
  #   indices = c("rx1day"),
  #   freq = "monthly")))
  
})
  
  
  

