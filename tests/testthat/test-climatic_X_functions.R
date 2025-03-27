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