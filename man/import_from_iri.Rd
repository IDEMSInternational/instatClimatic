% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_from_IRI.R
\name{import_from_iri}
\alias{import_from_iri}
\title{Import Data from IRI (International Research Institute for Climate and Society)}
\usage{
import_from_iri(download_from, data_file, path, X1, X2, Y1, Y2, get_area_point)
}
\arguments{
\item{download_from}{The source from which to download the data. Supported values are "CHIRPS_V2P0", "TAMSAT", "NOAA_ARC2", "NOAA_RFE2", "NOAA_CMORPH_DAILY", "NOAA_CMORPH_3HOURLY", "NOAA_CMORPH_DAILY_CALCULATED", "NASA_TRMM_3B42".}

\item{data_file}{The specific data file to download from the selected source.}

\item{path}{The directory path where the downloaded file will be saved. If empty, the current working directory is used.}

\item{X1}{The starting longitude or X-coordinate of the desired data range.}

\item{X2}{The ending longitude or X-coordinate of the desired data range.}

\item{Y1}{The starting latitude or Y-coordinate of the desired data range.}

\item{Y2}{The ending latitude or Y-coordinate of the desired data range.}

\item{get_area_point}{Specifies whether the data should be downloaded for an "area" or a single "point".}
}
\value{
A list containing two elements:
\itemize{
\item The imported data as a data frame.
\item A data frame containing unique latitude and longitude values from the imported data.
}
}
\description{
This function imports data from various sources at the International Research Institute for Climate and Society (IRI) based on the specified parameters.
}
\examples{
# Import area data from CHIRPS_V2P0 source
# import_from_iri(download_from = "CHIRPS_V2P0", data_file = "daily_0p05",
#                 path = "data", X1 = -10, X2 = 10, Y1 = -10, Y2 = 10,
#                 get_area_point = "area")

# Import point data from TAMSAT source
# import_from_iri(download_from = "TAMSAT", data_file = "rainfall_estimates",
#                 path = "", X1 = -1, Y1 = 50, get_area_point = "point")

}
