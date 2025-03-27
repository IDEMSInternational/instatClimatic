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

