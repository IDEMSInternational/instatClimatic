#' Ghana Climate Dataset
#'
#' A sample daily climate dataset for Ghana covering two stations from 1958 to 1962.
#' Useful for testing or demonstration of climatic analyses.
#'
#' @format A data frame with observations from 1958-01-01 to 1962-12-31:
#' \describe{
#'   \item{station}{Station name. One of \code{"Saltpond"} or \code{"Tamale"}.}
#'   \item{date}{Date of observation (YYYY-MM-DD), from 1958-01-01 to 1962-12-31.}
#'   \item{year}{Year of observation.}
#'   \item{doy}{Day of the year (1–366).}
#'   \item{rainfall}{Daily precipitation (mm).}
#'   \item{min_temperature}{Daily minimum temperature (°C).}
#'   \item{max_temperature}{Daily maximum temperature (°C).}
#'   \item{sunshine_hours}{Daily sunshine duration (hours).}
#'   \item{windspeed}{Daily wind speed (units unspecified).}
#'   \item{rh_06_00}{Relative humidity at 06:00 (percentage).}
#'   \item{rh_09_00}{Relative humidity at 09:00 (percentage).}
#'   \item{rh_12_00}{Relative humidity at 12:00 (percentage).}
#'   \item{rh_15_00}{Relative humidity at 15:00 (percentage).}
#'   \item{dd}{Wind direction (degrees or compass code, if applicable).}
#'   \item{kts}{Wind speed (knots).}
#' }
#'
#' @examples
#' head(ghana)
"ghana"

#' Synthetic Climate Dataset
#'
#' A small synthetic dataset for testing climate index functions. It contains daily records from two imaginary weather stations over the years 1981 to 1983.
#'
#' The dataset includes daily maximum and minimum temperatures, precipitation, and metadata such as year and month for each observation. It is designed for use in unit tests and function examples in this package.
#'
#' @format A data frame with 2190 rows and 8 columns:
#' \describe{
#'   \item{station}{Station name (character). This is `"X001"`.}
#'   \item{date}{Date (Date class) from 1981-01-01 to 1983-12-31.}
#'   \item{year}{Year of observation (numeric), from `1981` to `1983`.}
#'   \item{month}{Month of observation (numeric), `1` to `12`.}
#'   \item{tmax}{Daily maximum temperature (numeric, in °C).}
#'   \item{tmin}{Daily minimum temperature (numeric, in °C).}
#'   \item{precip}{Daily precipitation (numeric, in mm).}
#' }
#'
#' @details
#' This synthetic dataset is not based on real climate data. It is created solely for testing and development of climate indices and is not intended for scientific analysis.
#'
#' @source Internally generated using `rnorm()` and `rexp()` functions in R.
#'
#' @examples
#' head(climdex_data)
#' table(climdex_data$station)
#' summary(climdex_data$tmax)
"climdex_data"
