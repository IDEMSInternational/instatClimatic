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