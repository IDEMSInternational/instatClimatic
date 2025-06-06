#' Format Trace Rainfall Values
#'
#' This function formats numeric rainfall values, replacing a specific small value
#' (typically used to represent trace amounts of rainfall) with a string (e.g., "tr").
#' All other values are formatted with a fixed number of decimal places.
#'
#' @param x A numeric vector of rainfall values.
#' @param tr_value A numeric value of what to turn into "tr", default 0.03.
#'
#' @return A character vector where:
#' \itemize{
#'   \item `NA` values remain `NA`,
#'   \item values equal to `0.03` (interpreted as trace) are replaced with `"tr"`,
#'   \item other values are formatted to show at least two decimal places.
#' }
#'
#' @examples
#' fmt_trace(c(0, 0.03, 1.5, NA))
#' # Returns: "0.00" "tr" "1.50" NA
#'
#' @export
fmt_trace <- function(x, tr_value = 0.03) { 
  ifelse(is.na(x), NA,
         ifelse(x == 0.03, "tr", format(x, nsmall = 2)))  # could use Format(x) too here
}