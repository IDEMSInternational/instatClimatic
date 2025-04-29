#' Running spell length
#' 
#' Calculates the running number of consecutive non-zero values of a vector
#'
#' @param x A numeric vector
#' @param initial_value The initial value of the spell length
#'
#' @return a vector of length \code{x} of the running number of consecutive non-zero values of \code{x}
#' @export
#' 
#' @examples
#' # Basic use
#' spells(c(1, 1, 1, 0, 1, 1))
#' 
#' # With initial_value set to 10
#' spells(c(1, 1, 1, 0, 1, 1), initial_value = 10)
#' 
#' # All zeros
#' spells(c(0, 0, 0))
#' 
#' # Empty vector
#' spells(numeric(0))
#' 
#' # Starts with zero
#' spells(c(0, 1, 1))
spells <- function(x, initial_value = NA_real_) {
  y <- mat.or.vec(length(x), 1)
  if(length(x) > 0) {
    y[1] <- dplyr::if_else(x[1] == 0, 0, initial_value + 1)
    if(length(x) > 1) {
      for(i in 2:length(x)) {
        y[i] <- dplyr::if_else(x[i] == 0, 0, y[i - 1] + 1)
      }
    }
  }
  return(y)
}