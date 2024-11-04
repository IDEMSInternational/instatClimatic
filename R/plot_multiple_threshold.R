#' Multiple Threshold plots
#' 
#' @description
#' This function produces multiple threshold plots for various stations at a time.
#' 
#' @param data The input data frame containing the data for threshold plots.
#' @param station_col_name The name of the column in 'data' representing the station identifier.
#' @param element_col_name The name of the column in 'data' representing the element to plot.
#' @param r The threshold value for the element.
#' @param type The type of threshold plot to generate. Possible values: "GP" (Generalized Pareto), "PP" (Point Process), "Exponential".
#' @param nint The number of intervals for plotting.
#' @param alpha The significance level for threshold selection.
#' @param ncol The number of columns for arranging the plots.
#' @param xlb The label for the x-axis.
#' @param main The main title for the plot.
#' @param verbose Boolean value indicating whether to display verbose output.
#' @param ... Additional parameters to be passed to the threshold_Plot function.
#'
#' @return Returns a threshold plot
#' 
#' @export
#'
#' @examples # TODO
#' 
#' 
plot_multiple_threshold <- function(data, station_col_name, element_col_name, r, type = c("GP", "PP", "Exponential"), nint = 10,
                                    alpha = 0.05, ncol = 1, xlb = "", main = NULL , verbose = FALSE,...) {
  if (!missing(station_col_name)) {
    plts <- list()
    station_col <- data[, station_col_name]
    stations <- unique(station_col)
    for (i in seq_along(stations)) {
      d <- data[station_col == stations[i], ]
      element_col <- d[, element_col_name]
      plts[[i]] <- threshold_Plot(x = element_col, main = stations[i], r = r, type = type, nint = nint, alpha = alpha, verbose = verbose)
    }
    patchwork::wrap_plots(plts, ncol = ncol)
  }
  else {
    element_col <- data[, element_col_name]
    threshold_Plot(x = element_col, r = r, type = type, nint = nint, alpha = alpha, verbose = verbose)
  }
}