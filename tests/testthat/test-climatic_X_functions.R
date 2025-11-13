# climatic_details and climatic_inventory
ghana <- readRDS(file = "testdata/test_df.rds")

test_that("climatic_details works on minimal example", {
  # Run the function
  result <- climatic_details(
    data = ghana,
    date = date,
    elements = c("rainfall", "min_temperature"),
    stations = station
  )
  
  # Basic checks
  expect_s3_class(result, "data.frame")
  expect_true(all(c("From", "To", "Count", "Level") %in% names(result)))
  expect_true(all(result$Level == "Month"))
  expect_true(all(result$Element %in% c("rainfall", "min_temperature")))
})

test_that("climatic_details works on minimal example (month level)", {
  result <- climatic_details(
    data = ghana,
    date = date,
    elements = c("rainfall", "min_temperature"),
    stations = station,
    day = FALSE,
    month = TRUE,
    year = FALSE
  )
  
  expect_s3_class(result, "data.frame")
  expect_true("Month" %in% result$Level)
})

test_that("climatic_details returns day-level output correctly", {
  result <- climatic_details(
    data = ghana,
    date = date,
    elements = c("min_temperature"),
    stations = station,
    day = TRUE,
    month = FALSE,
    year = FALSE
  )
  
  expect_s3_class(result, "data.frame")
  expect_true("Day" %in% result$Level)
})

test_that("climatic_details returns year-level output correctly", {
  result <- climatic_details(
    data = ghana,
    date = date,
    elements = c("rainfall", "min_temperature"),
    stations = station,
    day = FALSE,
    month = FALSE,
    year = TRUE
  )
  
  expect_s3_class(result, "data.frame")
  expect_true("Year" %in% result$Level)
})

test_that("climatic_details warns if all levels are FALSE", {
  expect_warning(climatic_details(
    data = ghana,
    date = date,
    elements = c("rainfall", "min_temperature"),
    stations = station,
      day = FALSE,
      month = FALSE,
      year = FALSE,
    "At least one of day, month, year need to be selected"
  ))
})

test_that("climatic_details errors when date or elements are missing", {
  df <- data.frame(Date = Sys.Date(), Station = "A", Temp = NA)
  
  expect_error(
    climatic_details(
      data = ghana,
      elements = c("rainfall", "min_temperature"),
      stations = station
    ),
    'argument "date" is missing'
  )
  
  expect_error(
    climatic_details(
      data = ghana,
      date = date,
      stations = station
    ),
    'argument "elements" is missing'
  )
})

test_that("climatic_details works with multiple stations", {
  result <- climatic_details(
    data = ghana,
    date = date,
    elements = c("rainfall", "min_temperature"),
    stations = station,
    day = TRUE,
    month = FALSE,
    year = FALSE
  )
  
  expect_s3_class(result, "data.frame")
  expect_true(all(result$station %in% c("Saltpond", "Tamale")))
})

test_that("climatic_missing works with default start=TRUE and end=FALSE", {
  df <- data.frame(
    Date = seq.Date(as.Date("2020-01-01"), by = "day", length.out = 20),
    Station = rep("X", 20),
    Temp = c(rep(NA, 3), rnorm(17))
  )
  
  result <- climatic_missing(
    data = df,
    date = Date,
    elements = c(Temp),
    stations = Station
  )
  
  expect_s3_class(result, "data.frame")
  expect_equal(names(result), c("Station", "Element", "From", "To", "Missing", "%", "Full_Years"))
  expect_equal(unique(result$Element), "Temp")
})

test_that("climatic_missing works with start = FALSE and end = TRUE", {
  df <- data.frame(
    Date = seq.Date(as.Date("2020-01-01"), by = "day", length.out = 20),
    Station = rep("X", 20),
    Temp = c(rep(NA, 5), rnorm(15))
  )
  
  result <- climatic_missing(
    data = df,
    date = Date,
    elements = c(Temp),
    stations = Station,
    start = FALSE,
    end = TRUE
  )
  
  expect_true(result$From == min(df$Date))
  expect_true(result$To <= max(df$Date))
})

test_that("climatic_missing errors when date or elements are missing", {
  df <- data.frame(Date = Sys.Date(), Station = "A", Temp = NA)
  
  expect_error(
    climatic_missing(
      data = df,
      elements = c(Temp),
      stations = Station
    ),
    'argument "date" is missing'
  )
  
  expect_error(
    climatic_missing(
      data = df,
      date = Date,
      stations = Station
    ),
    'argument "elements" is missing'
  )
})

test_that("climatic_missing works with multiple stations", {
  df <- data.frame(
    Date = rep(seq.Date(as.Date("2020-01-01"), by = "day", length.out = 10), 2),
    Station = rep(c("A", "B"), each = 10),
    Temp = c(rep(NA, 3), rnorm(7), rnorm(10))
  )
  
  result <- climatic_missing(
    data = df,
    date = Date,
    elements = c(Temp),
    stations = Station
  )
  
  expect_s3_class(result, "data.frame")
  expect_true(all(result$Station %in% c("A", "B")))
})

test_that("climatic_missing works when there are no missing values", {
  df <- data.frame(
    Date = seq.Date(as.Date("2020-01-01"), by = "day", length.out = 30),
    Station = rep("X", 30),
    Temp = rnorm(30)
  )
  
  result <- climatic_missing(
    data = df,
    date = Date,
    elements = c(Temp),
    stations = Station
  )
  
  expect_equal(result$Missing, 0)
  expect_equal(result$`%`, 0)
  expect_true(result$Full_Years >= 0)
})


test_that("climatic_missing works when there are no missing values", {
  df <- data.frame(
    Date = c(seq.Date(as.Date("2020-01-01"), by = "day", length.out = 30), as.Date("2019-01-01")),
    Station = rep("X", 31),
    Temp = rnorm(31, 10)
  )
  
  result <- climatic_missing(
    data = df,
    date = Date,
    elements = c(Temp),
    stations = Station
  )
  
  expect_equal(result$Missing, 0)
  expect_equal(result$`%`, 0)
  expect_true(result$Full_Years >= 0)
})

test_that("climatic_details duplicates='keep' returns all levels with no duplicate_index", {
  # full year, all NA → we expect same spell at Day, Month, Year
  dates <- seq(as.Date("2000-01-01"), as.Date("2000-12-31"), by = "day")
  test_data <- data.frame(
    station = "TEST",
    date    = dates,
    sunh    = NA_real_
  )
  
  res <- climatic_details(
    data      = test_data,
    date      = date,
    elements  = c(sunh),
    stations  = station,
    day       = TRUE,
    month     = TRUE,
    year      = TRUE,
    duplicates = "keep"
  )
  
  # Should have three rows: one Day spell, one Month spell, one Year spell
  expect_equal(nrow(res), 3L)
  expect_false("duplicate_index" %in% names(res))
  expect_setequal(as.character(res$Level), c("Day", "Month", "Year"))
})

test_that("climatic_details duplicates='drop' keeps only one spell and omits duplicates", {
  dates <- seq(as.Date("2000-01-01"), as.Date("2000-12-31"), by = "day")
  test_data <- data.frame(
    station = "TEST",
    date    = dates,
    sunh    = NA_real_
  )
  
  expect_message(
    res <- climatic_details(
      data      = test_data,
      date      = date,
      elements  = c(sunh),
      stations  = station,
      day       = TRUE,
      month     = TRUE,
      year      = TRUE,
      duplicates = "distinct"
    ),
    "rows in details omitted as duplicates"
  )
  
  # Only a single spell for that station/element/date range
  expect_equal(nrow(res), 1L)
  expect_equal(as.character(res$Level), "Year") # Year is prioritised
  expect_false("duplicate_index" %in% names(res))
})

test_that("climatic_details duplicates='flag' keeps all spells and flags duplicates", {
  dates <- seq(as.Date("2000-01-01"), as.Date("2000-12-31"), by = "day")
  test_data <- data.frame(
    station = "TEST",
    date    = dates,
    sunh    = NA_real_
  )
  
  expect_message(
    res <- climatic_details(
      data      = test_data,
      date      = date,
      elements  = c(sunh),
      stations  = station,
      day       = TRUE,
      month     = TRUE,
      year      = TRUE,
      duplicates = "flag"
    ),
    "rows in details flagged as duplicates"
  )
  
  # Still three spells: Day, Month, Year
  expect_equal(nrow(res), 3L)
  expect_true("duplicate_index" %in% names(res))
  
  # First in each (station, Element, From, To) group has index 1
  expect_true(any(res$duplicate_index == 1L))
  expect_true(any(res$duplicate_index > 1L))
})

test_that("partial overlaps are not treated as duplicates", {
  # Missing from 20 March to 30 April: one long daily spell
  dates <- seq(as.Date("2000-01-01"), as.Date("2000-12-31"), by = "day")
  sunh  <- rep(0, length(dates))
  
  # Make 20 Mar to 30 Apr NA
  sunh[dates >= as.Date("2000-03-20") & dates <= as.Date("2000-04-30")] <- NA_real_
  
  test_data <- data.frame(
    station = "TEST",
    date    = dates,
    sunh    = sunh
  )
  
  res <- climatic_details(
    data      = test_data,
    date      = date,
    elements  = c(sunh),
    stations  = station,
    day       = TRUE,
    month     = TRUE,
    year      = FALSE,
    duplicates = "distinct"
  )
  
  # We expect:
  # - one daily spell from 20 March to 30 April
  # - one (or more) monthly spells with different From/To
  # But importantly: no identical (station, Element, From, To), so no omissions.
  
  # No message about omitted duplicates
  # (we don't wrap in expect_message here, so this will fail if a message appears)
  expect_gte(nrow(res), 2L)
  
  # Within each group of station/Element/From/To there should be single row
  res_grouped <- res %>%
    dplyr::group_by(station, Element, From, To) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop")
  
  expect_true(all(res_grouped$n == 1L))
})

test_that("climatic_details with duplicates = 'hierarchical' gives a clean year-month-day hierarchy", {
  # Build synthetic daily data: 2000-01-01 to 2002-12-31
  dates <- seq(as.Date("2000-01-01"), as.Date("2002-12-31"), by = "day")
  
  sunh <- rep(0, length(dates))
  
  # Make 2000-01-01 to 2001-12-31 fully missing (2 full years)
  sunh[dates >= as.Date("2000-01-01") & dates <= as.Date("2001-12-31")] <- NA_real_
  
  # Make 2002-01-01 to 2002-03-31 fully missing (3 full months)
  sunh[dates >= as.Date("2002-01-01") & dates <= as.Date("2002-03-31")] <- NA_real_
  
  # Make 2002-04-10 to 2002-04-20 missing (11 individual days)
  sunh[dates >= as.Date("2002-04-10") & dates <= as.Date("2002-04-20")] <- NA_real_
  
  test_data <- data.frame(
    station = "TEST",
    date    = dates,
    sunh    = sunh
  )
  
  # Run climatic_details at all three levels with hierarchical compression
  res <- suppressWarnings(climatic_details(
    data      = test_data,
    date      = date,
    elements  = c(sunh),
    stations  = station,
    day       = TRUE,
    month     = TRUE,
    year      = TRUE,
    duplicates = "hierarchical"
  ))
  
  # We expect exactly three spells: Year, Month, Day (in that order)
  expect_equal(nrow(res), 3L)
  expect_equal(as.character(res$Level), c("Year", "Month", "Day"))
  
  # Check date ranges
  expect_true(res$From[1] == as.Date("2000-01-01"))
  expect_true(res$From[2] == as.Date("2002-01-01"))
  expect_true(res$From[3] == as.Date("2002-04-10"))
  
  expect_true(res$To[1] == as.Date("2001-12-31"))
  expect_true(res$To[2] == as.Date("2002-03-31"))
  expect_true(res$To[3] == as.Date("2002-04-20"))

  # Check counts (2 years, 3 months, 11 days)
  expect_equal(as.numeric(res$Count), c(2L, 3L, 11L))
  
  # Station and element should be consistent
  expect_true(all(res$station == "TEST"))
  expect_true(all(as.character(res$Element) == "sunh"))
})