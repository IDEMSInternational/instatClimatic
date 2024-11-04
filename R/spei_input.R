#' Calculate SPEI Input Data
#' 
#' @description
#' This function calculates the Standardized Precipitation-Evapotranspiration Index (SPEI) input data from a given data frame. The function sorts the data by year and month, checks for data completeness, and prepares the data for further SPEI calculations.
#' 
#' #' @details
#' The function expects the input data to be in a data frame format. It calculates the SPEI input data by performing the following steps:
#' 
#' 1. Sorts the data frame by the year and month columns in ascending order.
#' 2. Verifies if the sorted data frame matches the original data frame, ensuring correct sorting for SPEI/SPI calculations. If they differ, an error is raised.
#' 3. Checks if there are multiple values per month (per station) in the data frame. If multiple values are detected, an error is raised as SPEI/SPI requires monthly data with one value per month.
#' 4. If the `station` parameter is provided, the function performs additional checks for each unique station:
#'    - Filters the data frame to include only rows with the current station.
#'    - Generates a sequence of dates from the first date in the filtered data to the last date, incrementing by one month.
#'    - Compares the length of the generated date sequence with the number of rows in the filtered data. If they differ, an error is raised, indicating missing months in the data for the current station.
#' 5. Constructs the `cols` variable by combining the `id_cols` and `element` columns.
#' 6. Determines the start year and month based on the first values in the `year` and `month` columns.
#' 7. If the `station` parameter is provided, the function transforms the data frame into a "wide" format using `pivot_wider`, organizing the data by year and month with station-specific columns. Missing values are filled with `NA`.
#' 8. Converts the resulting data frame to a time series object (`ts`) using `as.matrix`. The frequency is set to 12 for monthly data, and the start year and month are determined from the data.
#' 9. If the `station` parameter is not provided, the function directly converts the `element` column to a time series object (`ts`) using `as.matrix`, with a frequency of 12 and the determined start year and month.
#' 10. Returns the calculated time series data as the output.
#' 
#' #' @keywords
#' SPEI, precipitation, evapotranspiration, time series, data preparation
#'  
#' @param data The data.frame to calculate from.
#' @param station The name of the station column in \code{data}, if the data are for multiple station.
#' @param year The name of the year column in \code{data}.
#' @param month The name of the month column in \code{data}.
#' @param element The name of the column(s) in \code{data} to apply the condition to.
#'
#' @return A time series object (\code{ts}) containing the calculated SPEI input data.
#'
#' @export
#' 
#' @examples # TODO
#' 
#' 
spei_input <- function(data, station, year, month, element) {
  if (missing(station)) id_cols <- c(year, month) else id_cols <- c(station, year, month)
  # SPEI package assumes data is ordered so must be sorted
  data_sort <- data %>% dplyr::arrange(!!! rlang::syms(id_cols))
  data <- data_sort
  # There should be a better way to check this.
  if (!all(data == data_sort, na.rm = TRUE)) stop("data must be sorted by (", paste(id_cols, collapse = ", "), ") for SPEI/SPI to be calculated correctly.")
  # Monthly data i.e. one value per month (per station) is required
  if (anyDuplicated(data %>% dplyr::select(!!! rlang::syms(id_cols)))) stop("Multiple values per month detected. SPEI/SPI requires monthly data.")
  if (!missing(station)) {
    for (s in unique(data[[station]])) {
      df <- data %>% dplyr::filter(.data[[station]] == s)
      dates_seq <- seq.Date(from = as.Date(paste(df[[year]][1], as.numeric(df[[month]][1]), 1), format = "%Y %m %d"),
                            to = as.Date(paste(utils::tail(df[[year]], 1), utils::tail(as.numeric(df[[month]]), 1), 1), format = "%Y %m %d"),
                            by = "1 month")
      if (length(dates_seq) != nrow(df)) stop("Less rows than expected. data has gaps for missing months in '", s, "'. SPEI/SPI requires no date gaps.")
    }
  } else {
    dates_seq <- seq.Date(from = as.Date(paste(data[[year]][1], as.numeric(data[[month]][1]), 1), format = "%Y %m %d"),
                          to = as.Date(paste(utils::tail(data[[year]], 1), utils::tail(as.numeric(data[[month]]), 1), 1), format = "%Y %m %d"),
                          by = "1 month")
    if (length(dates_seq) != nrow(data)) stop("Less rows than expected. data has gaps for missing months. SPEI/SPI requires no date gaps.")
  }
  cols <- c(id_cols, element)
  start <- c(data[[year]][1], data[[month]][1])
  # If multiple stations, needs to be in "wide" format for SPEI
  if (!missing(station)) {
    ts_data <- tidyr::pivot_wider(data, id_cols = tidyselect::all_of(c(year, month)), 
                                  names_from = tidyselect::all_of(station), values_from = tidyselect::all_of(element),
                                  values_fill = NA)
    ts_data <- ts_data %>% dplyr::arrange(!!! rlang::syms(c(year, month)))
    # Not sure how to do this using dplyr::select
    ts_data[id_cols] <- NULL
    ts_data <- stats::ts(as.matrix(ts_data), frequency = 12, start = start)
  } else {
    ts_data <- stats::ts(as.matrix(data[[element]]), frequency = 12, start = start)
  }
  ts_data
}