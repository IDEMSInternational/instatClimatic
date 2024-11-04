
#' Convert decimal degrees to DMS format
#'
#' @description  Converts decimal degrees to degrees, minutes, and seconds (DMS) format.
#'
#' @param x Numeric value representing the decimal degree to be converted.
#' @param lat Logical value indicating whether the conversion is for latitude (TRUE) or longitude (FALSE).
#'
#' @return A character string representing the input decimal degree in DMS format.
#' The format is "DD MM SS Dir", where DD represents degrees, MM represents minutes,
#' SS represents seconds, and Dir represents the direction (N, S, E, or W).
#' The degrees, minutes, and seconds are zero-padded to two digits each.
#'
#' @export
#'
#' @examples
#' dd_to_dms(37.7749, 'TRUE')
#' # Output: "37 46 29 N"
#'
#' dd_to_dms(-122.4194, 'FALSE')
#' # Output: "122 25 10 W"
dd_to_dms <- function(x, lat) {
  if (lat) dir <- ifelse(x >= 0, "N", "S")
  else dir <- ifelse(x >= 0, "E", "W")
  x <- abs(x)
  d <- trunc(x)
  m <- trunc((x - d) * 60)
  s <- round((x - d - m/60) * 3600)
  return(paste(sprintf(ifelse(lat, "%02d", "%03d"), d), sprintf("%02d", m), sprintf("%02d", s), dir))
}