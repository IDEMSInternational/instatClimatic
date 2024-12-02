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
#' @export
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
#' @export
summary_sum <- function (x, weights = NULL, na.rm = FALSE, na_type = "", ...) {
  if(na.rm && na_type != "" && !na_check(x, na_type = na_type, ...)) return(NA)
  else {
    if (missing(weights) || is.null(weights)) return(sum(x, na.rm = na.rm))
    else return(sum(x * weights, na.rm = na.rm))
  }
}

#' Check Missing Values Based on Conditions
#'
#' Evaluates a vector against specified conditions for missing values.
#'
#' @param x A vector to check for missing values.
#' @param na_type A character vector specifying the types of checks to perform. Options include:
#'   \itemize{
#'     \item `"n"`: Total number of missing values (`<= na_max_n`).
#'     \item `"prop"`: Proportion of missing values (`<= na_max_prop` in percentage).
#'     \item `"n_non_miss"`: Minimum number of non-missing values (`>= na_min_n`).
#'     \item `"FUN"`: A custom function to evaluate missing values.
#'     \item `"con"`: Maximum consecutive missing values (`<= na_consecutive_n`).
#'   }
#' @param na_consecutive_n Optional. Maximum allowed consecutive missing values.
#' @param na_max_n Optional. Maximum allowed missing values.
#' @param na_max_prop Optional. Maximum allowed proportion of missing values (in percentage).
#' @param na_min_n Optional. Minimum required non-missing values.
#' @param na_FUN Optional. A custom function to evaluate missing values.
#' @param ... Additional arguments passed to the custom function `na_FUN`.
#' @return Logical. Returns `TRUE` if all specified checks pass, otherwise `FALSE`.
#' @export
na_check <- function(x, na_type = c(), na_consecutive_n = NULL, na_max_n = NULL, na_max_prop = NULL, na_min_n = NULL, na_FUN = NULL, ...) {
  res <- c()
  for (i in seq_along(na_type)) {
    type <- na_type[i]
    if (type %in% c("n","'n'")) {
      res[i] <- summary_count_missing(x) <= na_max_n
    }
    else if (type %in% c("prop","'prop'")) {
      res[i] <- (summary_count_missing(x) / summary_count(x)) <= na_max_prop / 100
    }
    else if (type %in% c("n_non_miss","'n_non_miss'")) {
      res[i] <- summary_count_non_missing(x) >= na_min_n
    }
    else if (type %in% c("FUN","'FUN'")) {
      res[i] <- na_FUN(x, ...)
    }
    else if (type %in% c("con","'con'")) {
      is_na_rle <- rle(is.na(x))
      res[i] <- max(is_na_rle$lengths[is_na_rle$values]) <= na_consecutive_n
    }
    else {
      stop("Invalid na_type specified for missing values check.")
    }
    if (!res[i]) {
      return(FALSE)
    }
  }
  return(all(res))
}