#' Calculate Mean of Data
#'
#' Computes the mean or weighted mean of a dataset.
#'
#' @param x A numeric vector.
#' @param add_cols Additional columns (not used directly in calculation).
#' @param weights Optional weights for the data.
#' @param na.rm Logical. Should missing values be removed? Defaults to `FALSE`.
#' @param trim Numeric. Fraction of observations to trim from each end before computing the mean.
#' @param na_type Character string indicating the type of NA check to perform.
#' @param ... Additional arguments passed to `na_check`.
#' @return The mean or weighted mean of the data.
summary_mean <- function (x, add_cols, weights = NULL, na.rm = FALSE, trim = 0, na_type = "", ...) {
  if( length(x)==0 || (na.rm && length(x[!is.na(x)])==0) ) return(NA)
  else {
    if(na.rm && na_type != "" && !na_check(x, na_type = na_type, ...)) return(NA)
    else {
      if (missing(weights) || is.null(weights))
        return(mean(x, na.rm = na.rm, trim = trim))
      else 
        return(stats::weighted.mean(x, w = weights, na.rm = na.rm))
    }
  }
}


#' Calculate Sum of Data
#'
#' Computes the sum or weighted sum of a dataset.
#'
#' @param x A numeric vector.
#' @param weights Optional weights for the data.
#' @param na.rm Logical. Should missing values be removed? Defaults to `FALSE`.
#' @param na_type Character string indicating the type of NA check to perform.
#' @param ... Additional arguments passed to `na_check`.
#' @return The sum or weighted sum of the data.
summary_sum <- function (x, weights = NULL, na.rm = FALSE, na_type = "", ...) {
  if(na.rm && na_type != "" && !na_check(x, na_type = na_type, ...)) return(NA)
  else {
    if (missing(weights) || is.null(weights)) return(sum(x, na.rm = na.rm))
    else return(sum(x * weights, na.rm = na.rm))
  }
}
