#' Retrieve minimum and maximum values of a dimension from a NetCDF file.
#' 
#' @description
#' Retrieves the minimum and maximum values of a dimension from a NetCDF file.
#' 
#' @param nc A NetCDF file object.
#' @param dimension The name of the dimension for which to retrieve the minimum and maximum values.
#' @param time_as_date A logical value indicating whether to treat time dimension values as dates. Default is TRUE.
#'
#' @return A numeric vector containing the minimum and maximum values of the dimension.
#' 
#' @export
#'
#' @examples
#' # nc_file <- nc_open("path/to/netcdf/file.nc")
#' # min_max <- nc_get_dim_min_max(nc_file, "time", time_as_date = TRUE)
#' # nc_close(nc_file)
#' 
#' 
 
nc_get_dim_min_max <- function(nc, dimension, time_as_date = TRUE) {
  if(!dimension %in% names(nc$dim)) stop(dimension, " not found in file.")
  vals <- nc$dim[[dimension]]$vals
  dim_axes <- ncdf4.helpers::nc.get.dim.axes(nc)
  time_dims <- names(dim_axes[which(dim_axes == "T")])
  if(dimension %in% time_dims && time_as_date) {
    time_vals <- c()
    try({
      units <- ncdf4::ncatt_get(nc, dimension, "units")
      if(units$hasatt && units$value == "julian_day") {
        # RDotNet interprets Date class as numeric so character needed to preserve date
        time_vals <- as.character(as.Date(vals, origin = structure(-2440588, class = "Date")))
      }
      else {
        pcict_time <- ncdf4.helpers::nc.get.time.series(nc, time.dim.name = dimension)
        posixct_time <- PCICt::as.POSIXct.PCICt(pcict_time)
        # RDotNet interprets Date class as numeric so character needed to preserve date
        time_vals <- as.character(as.Date(posixct_time))
      }
    })
    if(length(time_vals) > 0 && !anyNA(time_vals)) vals <- time_vals
  }
  bounds <- c(min(vals, na.rm = TRUE), max(vals, na.rm = TRUE))
  return(bounds)
}