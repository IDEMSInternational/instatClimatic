% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/climdex.R
\name{climdex}
\alias{climdex}
\title{Calculate Climate Indices}
\usage{
climdex(
  data,
  station,
  date,
  year = NULL,
  month = NULL,
  prec = NULL,
  tmax = NULL,
  tmin = NULL,
  indices,
  freq = "annual",
  base.range = c(1961, 1990),
  n = 5,
  northern.hemisphere = TRUE,
  quantiles = NULL,
  temp.qtiles = c(0.1, 0.9),
  prec.qtiles = c(0.95, 0.99),
  max.missing.days = c(annual = 15, monthly = 3, seasonal = 6),
  min.base.data.fraction.present = 0.1,
  spells.can.span.years = FALSE,
  gsl.mode = "GSL",
  threshold = 1
)
}
\arguments{
\item{data}{A data frame containing the temperature and precipitation data.}

\item{station}{The name of the column in the data frame that represents the station.}

\item{date}{The name of the column in the data frame that represents the date.}

\item{year}{The name of the column in the data frame that represents the year.}

\item{month}{The name of the column in the data frame that represents the month.}

\item{prec}{The name of the column in the data frame that represents precipitation (Default \code{NULL}).}

\item{tmax}{The name of the column in the data frame that represents maximum temperature (Default \code{NULL}).}

\item{tmin}{The name of the column in the data frame that represents minimum temperature (Default \code{NULL}).}

\item{indices}{A character vector specifying the climate indices to be calculated.}

\item{freq}{The frequency at which the indices should be calculated. Can be "annual" or "monthly". Defaults to "annual".}

\item{base.range}{A numeric vector specifying the base range for calculating indices. Defaults to c(1961, 1990).}

\item{n}{The number of years to be considered for calculating percentile-based indices. Defaults to 5.}

\item{northern.hemisphere}{A logical value indicating whether the data is from the Northern Hemisphere. Defaults to TRUE.}

\item{quantiles}{A numeric vector specifying the quantiles for calculating percentile-based indices. Defaults to NULL.}

\item{temp.qtiles}{A numeric vector specifying the quantiles for temperature-based indices. Defaults to c(0.1, 0.9).}

\item{prec.qtiles}{A numeric vector specifying the quantiles for precipitation-based indices. Defaults to c(0.95, 0.99).}

\item{max.missing.days}{A numeric vector specifying the maximum number of missing days allowed for each frequency. Defaults to c(annual = 15, monthly = 3).}

\item{min.base.data.fraction.present}{The minimum fraction of base data required for calculating indices. Defaults to 0.1.}

\item{spells.can.span.years}{A logical value indicating whether climate spells can span across years. Defaults to FALSE.}

\item{gsl.mode}{The method to calculate the growing season length. Can be "GSL" or "ASL". Defaults to "GSL".}

\item{threshold}{The threshold for calculating wet and dry spell indices. Defaults to 1.}
}
\value{
A data frame containing the calculated climate indices.
}
\description{
This function calculates climate indices based on temperature and precipitation data.
}
\examples{
# Example 1: Climdex indices return plausible values
data(climdex_data)
indices <- c("fd", "r10mm", "gsl", "cdd")

out <- climdex(
  data = climdex_data,
  date = "date",
  year = "year",
  month = "month",
  prec = "precip",
  tmax = "tmax",
  tmin = "tmin",
  indices = indices
)

out$fd
out$r10mm
out$gsl
out$cdd
  
# Example 2: Climdex works with southern hemisphere and different base range

indices <- c("gsl")
out <- climdex(
  data = climdex_data,
  station = "station",
  date = "date",
  year = "year",
  month = "month",
  prec = "precip",
  tmax = "tmax",
  tmin = "tmin",
  indices = indices,
  northern.hemisphere = FALSE,
  base.range = c(1981, 1982)
)

out

}
