% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-region.R
\name{plot_region}
\alias{plot_region}
\title{Generate a map of the selected product}
\usage{
plot_region(
  lon,
  lat,
  product,
  time,
  time_point = as.Date("2002-01-01"),
  add2title = "CM SAF, ",
  lonmin = NA,
  lonmax = NA,
  latmin = NA,
  latmax = NA,
  height = 600,
  width = 600,
  plot.ano = FALSE,
  set.col.breaks = FALSE,
  brk.set = seq(240, 310, 5),
  colmin0 = NA,
  colmax0 = NA,
  ncol = 14,
  plotHighRes = FALSE,
  plotCoastline = TRUE,
  plotCountries = TRUE,
  plotRivers = FALSE,
  contour.thick = 2,
  plotCities = TRUE,
  pch.cities = 2,
  cex.cities = 1,
  label.cities = TRUE,
  plotCapitals = 1,
  cex.label.cities = 0.5,
  dlat = 0.25,
  plotOwnLocations = FALSE,
  loc_lon = c(),
  loc_lat = c(),
  loc_name = c(""),
  label_pos = 1,
  variable = "Tm",
  level = 5,
  CTY.type = 4,
  ...
)
}
\arguments{
\item{lon}{Numeric vector representing the longitudes.}

\item{lat}{Numeric vector representing the latitudes.}

\item{product}{The data product to be plotted.}

\item{time}{A vector representing the time steps of the data.}

\item{time_point}{The specific time point to be plotted. Default is "2002-01-01".}

\item{add2title}{Additional text to be added to the plot title. Default is "CM SAF, ".}

\item{lonmin}{The minimum longitude value for the plot. Default is NA.}

\item{lonmax}{The maximum longitude value for the plot. Default is NA.}

\item{latmin}{The minimum latitude value for the plot. Default is NA.}

\item{latmax}{The maximum latitude value for the plot. Default is NA.}

\item{height}{The height of the plot in pixels. Default is 600.}

\item{width}{The width of the plot in pixels. Default is 600.}

\item{plot.ano}{If TRUE, plot the anomalies. Default is FALSE.}

\item{set.col.breaks}{If TRUE, set custom color breaks. Default is FALSE.}

\item{brk.set}{A numeric vector representing custom color breaks. Default is seq(240,310,5).}

\item{colmin0}{The minimum color value for the plot. Default is NA.}

\item{colmax0}{The maximum color value for the plot. Default is NA.}

\item{ncol}{The number of colors in the color scale. Default is 14.}

\item{plotHighRes}{If TRUE, plot the map in high resolution. Default is FALSE.}

\item{plotCoastline}{If TRUE, plot the coastline. Default is TRUE.}

\item{plotCountries}{If TRUE, plot the country borders. Default is TRUE.}

\item{plotRivers}{If TRUE, plot the rivers. Default is FALSE.}

\item{contour.thick}{The thickness of the contour lines. Default is 2.}

\item{plotCities}{If TRUE, plot the cities. Default is TRUE.}

\item{pch.cities}{The symbol type for plotting cities. Default is 2.}

\item{cex.cities}{The size of the city symbols. Default is 1.}

\item{label.cities}{If TRUE, label the cities. Default is TRUE.}

\item{plotCapitals}{The type of capitals to plot. Default is 1.}

\item{cex.label.cities}{The size of the city labels. Default is 0.5.}

\item{dlat}{The latitude spacing for plotting. Default is 0.25.}

\item{plotOwnLocations}{If TRUE, plot user-defined locations. Default is FALSE.}

\item{loc_lon}{Numeric vector representing the longitudes of the user-defined locations. Default is c().}

\item{loc_lat}{Numeric vector representing the latitudes of the user-defined locations. Default is c().}

\item{loc_name}{Character vector representing the names of the user-defined locations. Default is c("").}

\item{label_pos}{The position of the city labels. Default is 1.}

\item{variable}{The variable name. Default is "Tm".}

\item{level}{The level of the data. Default is 5.}

\item{CTY.type}{The data type for the "CTY" variable. Default is 4.}

\item{...}{Additional parameters to read in.}
}
\description{
This function generates a map of the selected product. It can plot either a certain year/month from a data file with multiple time steps or a single 2D field. The function requires the data to be prepared using the R script "Prep.Data.R" or "Apply.Function.R".
}
\examples{
# Example usage
# lon <- read.csv("lon.csv")
# lat <- read.csv("lat.csv")
# product <- read.csv("product.csv")
# time <- read.csv("time.csv")
# plot_region(lon, lat, product, time)

# Example usage with additional parameters
# plot_region(lon, lat, product, time, time_point = as.Date("2002-01-01"),
#             add2title = "CM SAF, ", lonmin = -10, lonmax = 10, latmin = 35,
#             latmax = 45, height = 800, width = 800, plot.ano = TRUE,
#             set.col.breaks = TRUE, brk.set = seq(240,310,10),
#             colmin0 = 240, colmax0 = 310, ncol = 10, plotHighRes = TRUE,
#             plotCoastline = FALSE, plotCountries = FALSE,
#             plotRivers = TRUE, contour.thick = 3, plotCities = FALSE,
#             pch.cities = 16, cex.cities = 1.5, label.cities = FALSE,
#             plotCapitals = 2, cex.label.cities = 0.8, dlat = 0.5,
#             plotOwnLocations = TRUE, loc_lon = c(0, 2, 4),
#             loc_lat = c(40, 42, 44), loc_name = c("Location A",
#             "Location B", "Location C"), label_pos = 3,
#             variable = "Precipitation", level = 2, CTY.type = 2)
}
