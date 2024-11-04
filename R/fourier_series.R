#' Fourier Series
#'
#' @description This function calculates the Fourier series for a given input `x` with a specified number of terms `n` and period `period`.
#'
#' @param x The input value for which the Fourier series is calculated.
#' @param n The number of terms in the Fourier series.
#' @param period The period of the Fourier series.
#'
#' @return The Fourier series expression as a character string.
#' @export
#'
#' @examples
#' fourier_series(3, 5, 2)
fourier_series <- function(x, n, period) {
  p2 <- "2 * pi"
  h <-  seq_len(n)
  paste0("sin(", x, " * ", h, " * ", p2, " / ", period, ")", " + ", 
         "cos(", x, " * ", h, " * ", p2, " / ", period, ")", 
         collapse = " + ")
}