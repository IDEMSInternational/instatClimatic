#' Wind pollution Rose
#' @description This function creates a wrapper around windRose and pollutionRose functions from openair package
#' @param mydata A data frame containing the data for the plot.
#' @param date_name The name of the column that contains the date information.
#' @param pollutant The name of the column that contains the pollutant information. 
#' @param type1_col_name  The name of the first type column.
#' @param type2_col_name  The name of the second type column.
#' @param ... Additional arguments to be passed to the underlying plotting functions.
#'
#' @return The generated wind or pollution rose plot.
#' @export
#'
#' @examples
#' # Example 1: Creating a wind rose plot
#' # wind_pollution_rose(mydata = wind_data, date_name = "date")
wind_pollution_rose <- function(mydata, date_name, pollutant, type1_col_name, type2_col_name, ...) {
  type <-  "default"
  if (!missing(type1_col_name) && !missing(type2_col_name)) {
    type <- c(type1_col_name, type2_col_name)
  }
  if (missing(type1_col_name) && !missing(type2_col_name)) {
    type <- type2_col_name
  }
  if (!missing(type1_col_name) && missing(type2_col_name)) {
    type <- type1_col_name
  }
  if (!("date" %in% colnames(mydata))) {
    mydata <- dplyr::rename(mydata, date = !!date_name)
  }
  if (missing(pollutant)) {
    openair::windRose(mydata = mydata, type = type, ...)
  } else {
    openair::pollutionRose(mydata = mydata, type = type, pollutant, ...)
  }
}