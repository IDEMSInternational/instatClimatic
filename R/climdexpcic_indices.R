#' #' Monthly Maximum 1-day Precipitation
#' #'
#' #' This function computes the climdex index Rx1day.
#' #'
#' #' This function takes a climdexInput object as input and computes the climdex
#' #' index Rx1day: monthly, seasonal or annual maximum 1-day precipitation.
#' #'
#' #' @param ci Object of type climdexInput.
#' #' @param freq Time frequency to aggregate to.
#' #' @param include.exact.dates Logical, if TRUE, return a data frame with the index values and exact dates for each month, season, or year; if FALSE, return only the index values.
#' #' @return A vector containing the value of the index per time interval.
#' #' If `include.exact.dates` is TRUE, a data frame with additional exact dates is returned.
#' #' @references This function is from the `pacificclimate/climdex.pcic` repository.
#' #' The `climdex.pcic` package was developed and maintained by the Pacific Climate Impacts Consortium (PCIC).
#' #' For more information, visit the repository: <https://github.com/pacificclimate/climdex.pcic>
#' #' Special thanks to the contributors of the `climdex.pcic` package for their efforts in climate data analysis.
#' climdex.rx1day <- function(ci, freq = c("monthly", "annual", "seasonal"), include.exact.dates = FALSE) {
#'   rxnday.params <- get.rxnday.params(ci, freq)
#'   ndays <- 1
#'   return(nday.consec.prec.max(daily.prec = rxnday.params$data, date.factor = rxnday.params$date.factor, ndays = ndays, include.exact.dates = include.exact.dates,  mask = rxnday.params$mask, freq = freq, cal = rxnday.params$cal))
#' }
#' 
#' #' Monthly Maximum 5-day Consecutive Precipitation
#' #'
#' #' This function computes the climdex index Rx5day.
#' #'
#' #' This function takes a climdexInput object as input and computes the climdex
#' #' index Rx5day: monthly, seasonal or annual maximum 5-day consecutive precipitation.
#' #'
#' #' @param ci Object of type climdexInput.
#' #' @param freq Time frequency to aggregate to.
#' #' @param center.mean.on.last.day Whether to center the 5-day running mean on
#' #' the last day of the window, instead of the center day.
#' #' @param include.exact.dates Logical, if TRUE, return a data frame with the index values and exact dates for each month, season, or year; if FALSE, return only the index values.
#' #' @return A vector containing the value of the index per time interval.
#' #' If `include.exact.dates` is TRUE, a data frame with additional exact dates is returned.
#' #' @references This function is from the `pacificclimate/climdex.pcic` repository.
#' #' The `climdex.pcic` package was developed and maintained by the Pacific Climate Impacts Consortium (PCIC).
#' #' For more information, visit the repository: <https://github.com/pacificclimate/climdex.pcic>
#' #' Special thanks to the contributors of the `climdex.pcic` package for their efforts in climate data analysis.
#' climdex.rx5day <- function(ci, freq = c("monthly", "annual", "seasonal"), center.mean.on.last.day = FALSE, include.exact.dates = FALSE) {
#'   rxnday.params <- get.rxnday.params(ci, freq)
#'   ndays <- 5
#'   return(nday.consec.prec.max(daily.prec = rxnday.params$data, date.factor = rxnday.params$date.factor, ndays = ndays, center.mean.on.last.day = center.mean.on.last.day, include.exact.dates = include.exact.dates, mask = rxnday.params$mask, freq = freq, cal = rxnday.params$cal))
#' }
#' 
#' #' Simple Precpitation Intensity Index
#' #'
#' #' This function computes the climdex index SDII.
#' #'
#' #' \code{climdex.sdii} computes the climdex index SDII, or Simple Precipitation
#' #' Intensity Index. This is defined as the sum of precipitation in wet days
#' #' (days with preciptitation over 1mm) during the year divided by the number of
#' #' wet days in the year.
#' #'
#' #' @param ci Object of type climdexInput.
#' #' @return A vector containing the value of the index for each year.
#' #' @note fclimdex rounds to 1 decimal place, whereas climdex.sdii does not.
#' #' This results in some small differences.
#' #' @references This function is from the `pacificclimate/climdex.pcic` repository.
#' #' The `climdex.pcic` package was developed and maintained by the Pacific Climate Impacts Consortium (PCIC).
#' #' For more information, visit the repository: <https://github.com/pacificclimate/climdex.pcic>
#' #' Special thanks to the contributors of the `climdex.pcic` package for their efforts in climate data analysis.
#' climdex.sdii <- function(ci) { stopifnot(!is.null(ci@data$prec)); return(simple.precipitation.intensity.index(ci@data$prec, ci@date.factors$annual) * ci@namasks$annual$prec) }
#' 
#' #' Precipitation Exceeding 10mm Per Day
#' #'
#' #' This function computes the climdex index R10mm.
#' #'
#' #' This function takes a climdexInput object as input and computes the climdex
#' #' index R10mm: the annual count of days where daily precipitation is more than 10mm per day.
#' #'
#' #' @param ci Object of type climdexInput.
#' #' @return A vector containing the value of the index for each year.
#' #' @references This function is from the `pacificclimate/climdex.pcic` repository.
#' #' The `climdex.pcic` package was developed and maintained by the Pacific Climate Impacts Consortium (PCIC).
#' #' For more information, visit the repository: <https://github.com/pacificclimate/climdex.pcic>
#' #' Special thanks to the contributors of the `climdex.pcic` package for their efforts in climate data analysis.
#' climdex.r10mm <- function(ci) { stopifnot(!is.null(ci@data$prec)); return(number.days.op.threshold(ci@data$prec, ci@date.factors$annual, 10, ">=") * ci@namasks$annual$prec) }
#' 
#' #' Precipitation Exceeding 20mm Per Day
#' #'
#' #' This function computes the climdex index R20mm.
#' #'
#' #' This function takes a climdexInput object as input and computes the climdex
#' #' index R20mm: the annual count of days where daily precipitation is more than 20mm per day.
#' #'
#' #' @param ci Object of type climdexInput.
#' #' @return A vector containing the value of the index for each year.
#' #' @references This function is from the `pacificclimate/climdex.pcic` repository.
#' #' The `climdex.pcic` package was developed and maintained by the Pacific Climate Impacts Consortium (PCIC).
#' #' For more information, visit the repository: <https://github.com/pacificclimate/climdex.pcic>
#' #' Special thanks to the contributors of the `climdex.pcic` package for their efforts in climate data analysis.
#' climdex.r20mm <- function(ci) { stopifnot(!is.null(ci@data$prec)); return(number.days.op.threshold(ci@data$prec, ci@date.factors$annual, 20, ">=") * ci@namasks$annual$prec) }
#' 
#' #' Precipitation Exceeding A Specified Amount Per Day
#' #'
#' #' This function computes the climdex index Rnnmm.
#' #'
#' #' This function takes a climdexInput object as input and computes the climdex
#' #' index Rnnmm: the annual count of days where daily precipitation is more than \code{nn} mm per day.
#' #'
#' #' @param ci Object of type climdexInput.
#' #' @param threshold The threshold to be used for Rnnmm.
#' #' @return A vector containing the value of the index for each year.
#' #' @references This function is from the `pacificclimate/climdex.pcic` repository.
#' #' The `climdex.pcic` package was developed and maintained by the Pacific Climate Impacts Consortium (PCIC).
#' #' For more information, visit the repository: <https://github.com/pacificclimate/climdex.pcic>
#' #' Special thanks to the contributors of the `climdex.pcic` package for their efforts in climate data analysis.
#' climdex.rnnmm <- function(ci, threshold=1) {
#'   stopifnot(!is.null(ci@data$prec));
#'   if(!is.numeric(threshold) || length(threshold) != 1) stop("Please specify a single numeric threshold value.");
#' 
#'   return(number.days.op.threshold(ci@data$prec, ci@date.factors$annual, threshold, ">=") * ci@namasks$annual$prec)
#' }
#' 
#' #' Maximum Consecutive Dry Days
#' #'
#' #' This function computes the climdex index CDD.
#' #'
#' #' This function computes the climdex index CDD: the annual maximum length of dry spells, in days.
#' #' Dry spells are considered to be sequences of days where daily preciptation
#' #' is less than 1mm per day.
#' #' @param ci Object of type climdexInput.
#' #' @param spells.can.span.years specifies whether spells can cross
#' #' year boundaries -- i.e., span years. The default for this is the same as
#' #' fclimdex.
#' #' @param include.exact.dates Logical, if TRUE, return a data frame with spell durations and the start and end dates of each spell; if FALSE, return only the spell durations.
#' #' @references This function is from the `pacificclimate/climdex.pcic` repository.
#' #' The `climdex.pcic` package was developed and maintained by the Pacific Climate Impacts Consortium (PCIC).
#' #' For more information, visit the repository: <https://github.com/pacificclimate/climdex.pcic>
#' #' Special thanks to the contributors of the `climdex.pcic` package for their efforts in climate data analysis.
#' climdex.cdd <- function(ci, spells.can.span.years = T, include.exact.dates = FALSE) {
#'   stopifnot(!is.null(ci@data$prec))
#'   return(spell.length.max(ci@data$prec, ci@date.factors$annual, 1, "<", spells.can.span.years, include.exact.dates, ci@namasks$annual$prec, attr(ci@dates, "cal")))
#' }
#' 
#' #' Maximum Consecutive Wet Days
#' #'
#' #' This function computes the climdex index CWD.
#' #'
#' #' This function takes a climdexInput object as input and computes the climdex
#' #' index CWD: the annual maximum length of wet spells, in days.
#' #' Wet spells are considered to be sequences of days where daily precipitation
#' #' is at least 1mm per day.
#' #' @param ci Object of type climdexInput.
#' #' @param spells.can.span.years specifies whether spells can cross
#' #' year boundaries -- i.e., span years. The default for this is the same as
#' #' fclimdex.
#' #' @param include.exact.dates Logical, if TRUE, return a data frame with spell durations and the start and end dates of each spell; if FALSE, return only the spell durations.
#' #' @references This function is from the `pacificclimate/climdex.pcic` repository.
#' #' The `climdex.pcic` package was developed and maintained by the Pacific Climate Impacts Consortium (PCIC).
#' #' For more information, visit the repository: <https://github.com/pacificclimate/climdex.pcic>
#' #' Special thanks to the contributors of the `climdex.pcic` package for their efforts in climate data analysis.
#' climdex.cwd <- function(ci, spells.can.span.years = T, include.exact.dates = FALSE) {
#'   stopifnot(!is.null(ci@data$prec))
#'   return(spell.length.max(ci@data$prec, ci@date.factors$annual, 1, ">=", spells.can.span.years, include.exact.dates, ci@namasks$annual$prec, attr(ci@dates, "cal")))
#' }
#' 
#' #' Total Daily Precipitation Exceeding 95\\%ile Threshold
#' #'
#' #' This function computes the climdex index R95pTOT.
#' #'
#' #' This function takes a climdexInput object as input and computes the climdex
#' #' index R95pTOT: the annual sum of precipitation in days where daily precipitation exceeds the
#' #' 95th percentile of daily precipitation in the base period.
#' #'
#' #' @param ci Object of type climdexInput.
#' #' @return A vector containing an annual timeseries of precipitation exceeding
#' #' the threshold.
#' #' @references This function is from the `pacificclimate/climdex.pcic` repository.
#' #' The `climdex.pcic` package was developed and maintained by the Pacific Climate Impacts Consortium (PCIC).
#' #' For more information, visit the repository: <https://github.com/pacificclimate/climdex.pcic>
#' #' Special thanks to the contributors of the `climdex.pcic` package for their efforts in climate data analysis.
#' climdex.r95ptot <- function(ci) { stopifnot(!is.null(ci@data$prec) && !is.null(ci@quantiles$prec)); return(total.precip.op.threshold(ci@data$prec, ci@date.factors$annual, ci@quantiles$prec['q95'], ">") * ci@namasks$annual$prec) }
#' 
#' #' Total Daily Precipitation Exceeding 99\\%ile Threshold
#' #'
#' #' This function computes the climdex index R99pTOT.
#' #'
#' #' This function takes a climdexInput object as input and computes the climdex
#' #' index R99pTOT: the annual sum of precipitation in days where daily precipitation exceeds the
#' #' 99th percentile of daily precipitation in the base period.
#' #'
#' #' @param ci Object of type climdexInput.
#' #' @return A vector containing an annual timeseries of precipitation exceeding
#' #' the threshold.
#' #' @references This function is from the `pacificclimate/climdex.pcic` repository.
#' #' The `climdex.pcic` package was developed and maintained by the Pacific Climate Impacts Consortium (PCIC).
#' #' For more information, visit the repository: <https://github.com/pacificclimate/climdex.pcic>
#' #' Special thanks to the contributors of the `climdex.pcic` package for their efforts in climate data analysis.
#' climdex.r99ptot <- function(ci) { stopifnot(!is.null(ci@data$prec) && !is.null(ci@quantiles$prec)); return(total.precip.op.threshold(ci@data$prec, ci@date.factors$annual, ci@quantiles$prec['q99'], ">") * ci@namasks$annual$prec) }
#' 
#' #' Total Daily Precipitation
#' #'
#' #' This function computes the climdex index PRCPTOT.
#' #'
#' #' This function takes a climdexInput object as input and computes the climdex
#' #' index PRCPTOT: the annual sum of precipitation in wet days
#' #' (days where precipitation is at least 1mm).
#' #'
#' #' @param ci Object of type climdexInput.
#' #' @return A vector containing an annual timeseries of precipitation in wet days.
#' #' @references This function is from the `pacificclimate/climdex.pcic` repository.
#' #' The `climdex.pcic` package was developed and maintained by the Pacific Climate Impacts Consortium (PCIC).
#' #' For more information, visit the repository: <https://github.com/pacificclimate/climdex.pcic>
#' #' Special thanks to the contributors of the `climdex.pcic` package for their efforts in climate data analysis.
#' climdex.prcptot <- function(ci) { stopifnot(!is.null(ci@data$prec)); return(total.precip.op.threshold(ci@data$prec, ci@date.factors$annual, 1, ">=") * ci@namasks$annual$prec) }
#' 
#' #' Frost Days
#' #'
#' #' This function computes the climdex index FD.
#' #'
#' #' This function takes a climdexInput object as input and computes the FD (frost
#' #' days) climdex index: that is, the annual count of days where daily minimum
#' #' temperature drops below 0 degrees Celsius.
#' #'
#' #' @param ci Object of type climdexInput.
#' #' @return A vector containing the number of frost days for each year.
#' #' @references This function is from the `pacificclimate/climdex.pcic` repository.
#' #' The `climdex.pcic` package was developed and maintained by the Pacific Climate Impacts Consortium (PCIC).
#' #' For more information, visit the repository: <https://github.com/pacificclimate/climdex.pcic>
#' #' Special thanks to the contributors of the `climdex.pcic` package for their efforts in climate data analysis.
#' climdex.fd <- function(ci) { stopifnot(!is.null(ci@data$tmin)); return(number.days.op.threshold(ci@data$tmin, ci@date.factors$annual, 0, "<") * ci@namasks$annual$tmin) }
#' 
#' #' Summer Days
#' #'
#' #' This function computes the climdex index SU.
#' #'
#' #' This function takes a climdexInput object as input and computes the SU (summer
#' #' days) climdex index: that is, the annual count of days where daily maximum
#' #' temperature exceeds 25 degrees Celsius.
#' #'
#' #' @param ci Object of type climdexInput.
#' #' @return A vector containing the number of summer days for each year.
#' #' @references This function is from the `pacificclimate/climdex.pcic` repository.
#' #' The `climdex.pcic` package was developed and maintained by the Pacific Climate Impacts Consortium (PCIC).
#' #' For more information, visit the repository: <https://github.com/pacificclimate/climdex.pcic>
#' #' Special thanks to the contributors of the `climdex.pcic` package for their efforts in climate data analysis.
#' climdex.su <- function(ci) { stopifnot(!is.null(ci@data$tmax)); return(number.days.op.threshold(ci@data$tmax, ci@date.factors$annual, 25, ">") * ci@namasks$annual$tmax) }
#' 
#' #' Icing Days
#' #'
#' #' This function computes the climdex index ID.
#' #'
#' #' This function takes a climdexInput object as input and computes the ID (icing
#' #' days) climdex index: that is, the annual count of days where daily maximum
#' #' temperature is below 0 degrees Celsius.
#' #'
#' #' @param ci Object of type climdexInput.
#' #' @return A vector containing the number of icing days for each year.
#' #' @references This function is from the `pacificclimate/climdex.pcic` repository.
#' #' The `climdex.pcic` package was developed and maintained by the Pacific Climate Impacts Consortium (PCIC).
#' #' For more information, visit the repository: <https://github.com/pacificclimate/climdex.pcic>
#' #' Special thanks to the contributors of the `climdex.pcic` package for their efforts in climate data analysis.
#' climdex.id <- function(ci) { stopifnot(!is.null(ci@data$tmax)); return(number.days.op.threshold(ci@data$tmax, ci@date.factors$annual, 0, "<") * ci@namasks$annual$tmax) }
#' 
#' #' Tropical Nights
#' #'
#' #' This function computes the climdex index TR.
#' #'
#' #' This function takes a climdexInput object as input and computes the TR
#' #' (tropical nights) climdex index: that is, the annual count of days where
#' #' daily minimum temperature stays above 20 degrees Celsius.
#' #'
#' #' @param ci Object of type climdexInput.
#' #' @return A vector containing the number of tropical nights for each year.
#' #' @references This function is from the `pacificclimate/climdex.pcic` repository.
#' #' The `climdex.pcic` package was developed and maintained by the Pacific Climate Impacts Consortium (PCIC).
#' #' For more information, visit the repository: <https://github.com/pacificclimate/climdex.pcic>
#' #' Special thanks to the contributors of the `climdex.pcic` package for their efforts in climate data analysis.
#' climdex.tr <- function(ci) { stopifnot(!is.null(ci@data$tmin)); return(number.days.op.threshold(ci@data$tmin, ci@date.factors$annual, 20, ">") * ci@namasks$annual$tmin) }
#' 
#' #' @title Growing Season Length
#' #'
#' #' @description
#' #' This function computes the growing season length (GSL) given the input.
#' #'
#' #' @details
#' #' This function takes a climdexInput object as input and computes the growing
#' #' season length based on this data.
#' #'
#' #' Growing season length as defined by the climdex indices is the number of
#' #' days between the start of the first spell of warm days in the first half of
#' #' the year, and the start of the first spell of cold days in the second half
#' #' of the year. Spells of warm days are defined as six or more days with mean
#' #' temperature above 5 degrees Celsius; spells of cold days are defined as six
#' #' or more days with a mean temperature below 5 degrees Celsius.
#' #'
#' #' The three alternate modes provided ('GSL_first', 'GSL_max', and 'GSL_sum')
#' #' are for testing purposes only. They differ considerably from the first
#' #' ('GSL') mode. All of them use a list of growing seasons -- here defined as
#' #' six or more consecutive days with a mean temperature greater than or equal
#' #' to 5 degrees Celsius, followed by either the end of the year or six or more
#' #' consecutive days with a mean temperature less than 5 degrees Celsius.
#' #' 'GSL_first' returns the first growing season found; 'GSL_max' returns the
#' #' longest growing season found; and 'GSL_sum' returns the total length of all
#' #' growing seasons found.
#' #'
#' #' @param ci Object of type climdexInput.
#' #' @param gsl.mode Growing season length method to use.
#' #' @param include.exact.dates Logical, if TRUE, return a data frame with the season lengths and the start and end dates of each season; if FALSE, return only the season lengths.
#' #' @return A vector containing the number of days in the growing season for
#' #' each year.
#' #' @note Note that fclimdex results may differ from results using the first
#' #' ('GSL') mode due to bugs in fclimdex. Please ensure you are using the latest
#' #' version of fclimdex, as there have been numerous bug fixes and the results
#' #' should, at this point, match.
#' #'
#' #' Please do not use the 'GSL_first', 'GSL_max', or 'GSL_sum' modes for
#' #' anything other than testing purposes at this time, nor should you rely on
#' #' this parameter being present in future versions of climdex.pcic.
#' #' @references \url{http://etccdi.pacificclimate.org/list_27_indices.shtml}
#' #' @keywords ts climate
#' #' @references This function is from the `pacificclimate/climdex.pcic` repository.
#' #' The `climdex.pcic` package was developed and maintained by the Pacific Climate Impacts Consortium (PCIC).
#' #' For more information, visit the repository: <https://github.com/pacificclimate/climdex.pcic>
#' #' Special thanks to the contributors of the `climdex.pcic` package for their efforts in climate data analysis.
#' climdex.gsl <- function(ci, gsl.mode = c("GSL", "GSL_first", "GSL_max", "GSL_sum"), include.exact.dates = FALSE) {
#'   stopifnot(!is.null(ci@data$tavg))
#'   ## Gotta shift dates so that July 1 is considered Jan 1 of same year in southern hemisphere
#'   if (ci@northern.hemisphere) {
#'     gs <- growing.season.length(ci@data$tavg, ci@date.factors$annual, ci@dates, ci@northern.hemisphere, gsl.mode = match.arg(gsl.mode), include.exact.dates = include.exact.dates, cal = attr(ci@dates, "cal"))
#'     namask.gsl <- ci@namasks$annual$tavg
#' 
#'   } else {
#'     dates.POSIXlt <- as.POSIXlt(ci@dates)
#'     years <- dates.POSIXlt$year + 1900
#'     months <- dates.POSIXlt$mon + 1
#' 
#'     valid.years <- range(years)
#'     years.gsl <- years - floor((12 - months) / 6)
#' 
#'     inset <- years.gsl >= valid.years[1]
#'     gsl.factor <- factor(years.gsl[inset])
#'     gsl.factor.monthly <- factor(paste(years.gsl[inset], months[inset], sep = "-"))
#'     gsl.yearmonth.factor <- unlist(strsplit(levels(gsl.factor.monthly), "-"))[(0:(nlevels(gsl.factor.monthly) - 1)) * 2 + 1]
#'     gsl.temp.data <- ci@data$tavg[inset]
#'     namask.gsl.monthly <- get.na.mask(gsl.temp.data, gsl.factor.monthly, ci@max.missing.days["annual"])
#'     namask.gsl <- get.na.mask(gsl.temp.data, gsl.factor, ci@max.missing.days["annual"]) * as.numeric(tapply(namask.gsl.monthly, gsl.yearmonth.factor, prod))
#'     dim(namask.gsl) <- dimnames(namask.gsl) <- NULL
#'     namask.gsl[length(namask.gsl)] <- NA
#' 
#'     gs <- growing.season.length(gsl.temp.data, gsl.factor, ci@dates[inset], ci@northern.hemisphere, gsl.mode = match.arg(gsl.mode), include.exact.dates = include.exact.dates, cal = attr(ci@dates, "cal"))
#'   }
#'   if (include.exact.dates) {
#'     gs$sl <- gs$sl * namask.gsl
#'     gs$start[is.na(gs$sl)] <- NA
#'     gs$end[is.na(gs$sl)] <- NA
#'     return(gs)
#'   }
#' 
#'   return(gs * namask.gsl)
#' }
#' 
#' #' @title Warm Spell Duration Index
#' #'
#' #' @description This function computes the climdex index WSDI.
#' #' @param ci Object of type climdexInput.
#' #' @param spells.can.span.years specifies whether spells can cross
#' #' year boundaries -- i.e., span years. The default for this is the same as
#' #' fclimdex.
#' #'
#' #' @details
#' #' This function takes a climdexInput object as input and computes the climdex
#' #' index WSDI (Warm Spell Duration Index).
#' #'
#' #' The warm spell duration index is defined as the number of days each year
#' #' which are part of a "warm spell". A "warm spell" is defined as a sequence of
#' #' 6 or more days in which the daily maximum temperature exceeds the 90th
#' #' percentile of daily maximum temperature for a 5-day running window
#' #' surrounding this day during the baseline period.
#' #'
#' #' @references This function is from the `pacificclimate/climdex.pcic` repository.
#' #' The `climdex.pcic` package was developed and maintained by the Pacific Climate Impacts Consortium (PCIC).
#' #' For more information, visit the repository: <https://github.com/pacificclimate/climdex.pcic>
#' #' Special thanks to the contributors of the `climdex.pcic` package for their efforts in climate data analysis.
#' climdex.wsdi <- function(ci, spells.can.span.years=FALSE) { stopifnot(!is.null(ci@data$tmax) && !is.null(ci@quantiles$tmax)); return(threshold.exceedance.duration.index(ci@data$tmax, ci@date.factors$annual, ci@jdays, ci@quantiles$tmax$outbase$q90, ">", spells.can.span.years=spells.can.span.years, max.missing.days=ci@max.missing.days['annual']) * ci@namasks$annual$tmax) }
#' 
#' #' @title Cold Spell Duration Index
#' #'
#' #' @description This function computes the climdex index CSDI.
#' #'
#' #' @param ci Object of type climdexInput.
#' #' @param spells.can.span.years specifies whether spells can cross
#' #' year boundaries -- i.e., span years. The default for this is the same as
#' #' fclimdex.
#' #'
#' #' @details
#' #' This function takes a climdexInput object as input and computes the climdex
#' #' index CSDI (Cold Spell Duration Index).
#' #'
#' #' The cold spell duration index is defined as the number of days
#' #' each year which are part of a "cold spell". A "cold spell" is defined as a
#' #' sequence of 6 or more days in which the daily minimum temperature is below
#' #' the 10th percentile of daily minimum temperature for a 5-day running window
#' #' surrounding this day during the baseline period.
#' #'
#' #' @references This function is from the `pacificclimate/climdex.pcic` repository.
#' #' The `climdex.pcic` package was developed and maintained by the Pacific Climate Impacts Consortium (PCIC).
#' #' For more information, visit the repository: <https://github.com/pacificclimate/climdex.pcic>
#' #' Special thanks to the contributors of the `climdex.pcic` package for their efforts in climate data analysis.
#' climdex.csdi <- function(ci, spells.can.span.years=FALSE) { stopifnot(!is.null(ci@data$tmin) && !is.null(ci@quantiles$tmin)); return(threshold.exceedance.duration.index(ci@data$tmin, ci@date.factors$annual, ci@jdays, ci@quantiles$tmin$outbase$q10, "<", spells.can.span.years=spells.can.span.years, max.missing.days=ci@max.missing.days['annual']) * ci@namasks$annual$tmin) }
#' 
#' #' Monthly Maximum of Daily Maximum Temperature
#' #'
#' #' This function computes the climdex index TXx.
#' #'
#' #' This function takes a climdexInput object as input and computes
#' #' the monthly, seasonal or annual maximum of daily maximum temperature.
#' #'
#' #' @param ci Object of type climdexInput.
#' #' @param freq Time frequency to aggregate to.
#' #' @param include.exact.dates Logical, if TRUE, return a data frame with the index values and exact dates for each month, season, or year; if FALSE, return only the index values.
#' #' @return A vector containing the value of the index per time interval.
#' #' If `include.exact.dates` is TRUE, a data frame with additional exact dates is returned.
#' #' @references This function is from the `pacificclimate/climdex.pcic` repository.
#' #' The `climdex.pcic` package was developed and maintained by the Pacific Climate Impacts Consortium (PCIC).
#' #' For more information, visit the repository: <https://github.com/pacificclimate/climdex.pcic>
#' #' Special thanks to the contributors of the `climdex.pcic` package for their efforts in climate data analysis.
#' climdex.txx <- function(ci, freq = c("monthly", "annual", "seasonal"), include.exact.dates = FALSE) {
#'   return(compute.stat(ci, "max", "tmax", freq, include.exact.dates))
#' }
#' 
#' #' Monthly Maximum of Daily Minimum Temperature
#' #'
#' #' This function computes the climdex index TNx.
#' #'
#' #' This function takes a climdexInput object as input and computes
#' #' the monthly, seasonal or annual maximum of daily minimum temperature.
#' #'
#' #' @param ci Object of type climdexInput.
#' #' @param freq Time frequency to aggregate to.
#' #' @param include.exact.dates Logical, if TRUE, return a data frame with the index values and exact dates for each month, season, or year; if FALSE, return only the index values.
#' #' @return A vector containing the value of the index per time interval.
#' #' If `include.exact.dates` is TRUE, a data frame with additional exact dates is returned.
#' #' @references This function is from the `pacificclimate/climdex.pcic` repository.
#' #' The `climdex.pcic` package was developed and maintained by the Pacific Climate Impacts Consortium (PCIC).
#' #' For more information, visit the repository: <https://github.com/pacificclimate/climdex.pcic>
#' #' Special thanks to the contributors of the `climdex.pcic` package for their efforts in climate data analysis.
#' climdex.tnx <- function(ci, freq = c("monthly", "annual", "seasonal"), include.exact.dates = FALSE) {
#'   return(compute.stat(ci, "max", "tmin", freq, include.exact.dates))
#' }
#' 
#' #' Monthly Minimum of Daily Maximum Temperature
#' #'
#' #' This function computes the climdex index TXn.
#' #'
#' #' This function takes a climdexInput object as input and computes
#' #' the monthly, seasonal or annual minimum of daily maximum temperature.
#' #'
#' #' @param ci Object of type climdexInput.
#' #' @param freq Time frequency to aggregate to.
#' #' @param include.exact.dates Logical, if TRUE, return a data frame with the index values and exact dates for each month, season, or year; if FALSE, return only the index values.
#' #' @return A vector containing the value of the index per time interval.
#' #' If `include.exact.dates` is TRUE, a data frame with additional exact dates is returned.
#' #' @references This function is from the `pacificclimate/climdex.pcic` repository.
#' #' The `climdex.pcic` package was developed and maintained by the Pacific Climate Impacts Consortium (PCIC).
#' #' For more information, visit the repository: <https://github.com/pacificclimate/climdex.pcic>
#' #' Special thanks to the contributors of the `climdex.pcic` package for their efforts in climate data analysis.
#' climdex.txn <- function(ci, freq = c("monthly", "annual", "seasonal"), include.exact.dates = FALSE) {
#'   return(compute.stat(ci, "min", "tmax", freq, include.exact.dates))
#' }
#' 
#' #' Monthly Minimum of Daily Minimum Temperature
#' #'
#' #' This function computes the climdex index TNn.
#' #'
#' #' This function takes a climdexInput object as input and computes
#' #' the monthly, seasonal or annual minimum of daily minimum temperature.
#' #'
#' #' @param ci Object of type climdexInput.
#' #' @param freq Time frequency to aggregate to.
#' #' @param include.exact.dates Logical, if TRUE, return a data frame with the index values and exact dates for each month, season, or year; if FALSE, return only the index values.
#' #' @return A vector containing the value of the index per time interval.
#' #' If `include.exact.dates` is TRUE, a data frame with additional exact dates is returned.
#' #' @references This function is from the `pacificclimate/climdex.pcic` repository.
#' #' The `climdex.pcic` package was developed and maintained by the Pacific Climate Impacts Consortium (PCIC).
#' #' For more information, visit the repository: <https://github.com/pacificclimate/climdex.pcic>
#' #' Special thanks to the contributors of the `climdex.pcic` package for their efforts in climate data analysis.
#' climdex.tnn <- function(ci, freq = c("monthly", "annual", "seasonal"), include.exact.dates = FALSE) {
#'   return(compute.stat(ci, "min", "tmin", freq, include.exact.dates))
#' }
#' 
#' ## Our implementation currently follows the example set by fclimdex for dealing with missing values, which is wrong; it biases results upwards when missing values are present.
#' 
#' #' Percent of Values Below 10th Percentile Daily Minimum Temperature
#' #'
#' #' This function computes the climdex index TN10p.
#' #'
#' #' This function takes a climdexInput object as input and computes the
#' #' monthly, seasonal or annual percent of values below the 10th percentile of baseline
#' #' daily minimum temperature.
#' #' @param ci Object of type climdexInput.
#' #' @param freq Time frequency to aggregate to.
#' #' @references This function is from the `pacificclimate/climdex.pcic` repository.
#' #' The `climdex.pcic` package was developed and maintained by the Pacific Climate Impacts Consortium (PCIC).
#' #' For more information, visit the repository: <https://github.com/pacificclimate/climdex.pcic>
#' #' Special thanks to the contributors of the `climdex.pcic` package for their efforts in climate data analysis.
#' climdex.tn10p <- function(ci, freq=c("monthly", "annual", "seasonal")) { stopifnot(!is.null(ci@data$tmin) && !is.null(ci@quantiles$tmin)); return(percent.days.op.threshold(ci@data$tmin, ci@dates, ci@jdays, ci@date.factors[[match.arg(freq)]], ci@quantiles$tmin$outbase$q10, ci@quantiles$tmin$inbase$q10, ci@base.range, "<", ci@max.missing.days[match.arg(freq)]) * ci@namasks[[match.arg(freq)]]$tmin) }
#' 
#' #' Percent of Values Below 10th Percentile Daily Maximum Temperature
#' #'
#' #' This function computes the climdex index TX10p.
#' #'
#' #' This function takes a climdexInput object as input and computes the
#' #' monthly, seasonal or annual percent of values below the 10th percentile of baseline
#' #' daily maximum temperature.
#' #' @param ci Object of type climdexInput.
#' #' @param freq Time frequency to aggregate to.
#' #' @references This function is from the `pacificclimate/climdex.pcic` repository.
#' #' The `climdex.pcic` package was developed and maintained by the Pacific Climate Impacts Consortium (PCIC).
#' #' For more information, visit the repository: <https://github.com/pacificclimate/climdex.pcic>
#' #' Special thanks to the contributors of the `climdex.pcic` package for their efforts in climate data analysis.
#' climdex.tx10p <- function(ci, freq=c("monthly", "annual", "seasonal")) { stopifnot(!is.null(ci@data$tmax) && !is.null(ci@quantiles$tmax)); return(percent.days.op.threshold(ci@data$tmax, ci@dates, ci@jdays, ci@date.factors[[match.arg(freq)]], ci@quantiles$tmax$outbase$q10, ci@quantiles$tmax$inbase$q10, ci@base.range, "<", ci@max.missing.days[match.arg(freq)]) * ci@namasks[[match.arg(freq)]]$tmax) }
#' 
#' #' Percent of Values Above 90th Percentile Daily Minimum Temperature
#' #'
#' #' This function computes the climdex index TN90p.
#' #'
#' #' This function takes a climdexInput object as input and computes the
#' #' monthly, seasonal or annual percent of values above the 90th percentile of baseline
#' #' daily minimum temperature.
#' #' @param ci Object of type climdexInput.
#' #' @param freq Time frequency to aggregate to.
#' #' @references This function is from the `pacificclimate/climdex.pcic` repository.
#' #' The `climdex.pcic` package was developed and maintained by the Pacific Climate Impacts Consortium (PCIC).
#' #' For more information, visit the repository: <https://github.com/pacificclimate/climdex.pcic>
#' #' Special thanks to the contributors of the `climdex.pcic` package for their efforts in climate data analysis.
#' climdex.tn90p <- function(ci, freq=c("monthly", "annual", "seasonal")) { stopifnot(!is.null(ci@data$tmin) && !is.null(ci@quantiles$tmin)); return(percent.days.op.threshold(ci@data$tmin, ci@dates, ci@jdays, ci@date.factors[[match.arg(freq)]], ci@quantiles$tmin$outbase$q90, ci@quantiles$tmin$inbase$q90, ci@base.range, ">", ci@max.missing.days[match.arg(freq)]) * ci@namasks[[match.arg(freq)]]$tmin) }
#' 
#' #' Percent of Values Above 90th Percentile Daily Maximum Temperature
#' #'
#' #' This function computes the climdex index TX90p.
#' #'
#' #' This function takes a climdexInput object as input and computes the
#' #' monthly, seasonal or annual percent of values above the 90th percentile of baseline
#' #' daily maximum temperature.
#' #' @param ci Object of type climdexInput.
#' #' @param freq Time frequency to aggregate to.
#' #' @references This function is from the `pacificclimate/climdex.pcic` repository.
#' #' The `climdex.pcic` package was developed and maintained by the Pacific Climate Impacts Consortium (PCIC).
#' #' For more information, visit the repository: <https://github.com/pacificclimate/climdex.pcic>
#' #' Special thanks to the contributors of the `climdex.pcic` package for their efforts in climate data analysis.
#' climdex.tx90p <- function(ci, freq=c("monthly", "annual", "seasonal")) { stopifnot(!is.null(ci@data$tmax) && !is.null(ci@quantiles$tmax)); return(percent.days.op.threshold(ci@data$tmax, ci@dates, ci@jdays, ci@date.factors[[match.arg(freq)]], ci@quantiles$tmax$outbase$q90, ci@quantiles$tmax$inbase$q90, ci@base.range, ">", ci@max.missing.days[match.arg(freq)]) * ci@namasks[[match.arg(freq)]]$tmax) }
#' 
#' #' Mean Diurnal Temperature Range
#' #'
#' #' This function computes the diurnal temperature range on a monthly basis.
#' #'
#' #' \code{climdex.dtr} computes the mean daily diurnal temperature range. The
#' #' frequency of observation can be either monthly or annual.
#' #'
#' #' @param ci Object of type climdexInput.
#' #' @param freq Time frequency to aggregate to.
#' #' @return A vector containing the mean monthly, mean seasonal or mean annual diurnal
#' #' temperature range.
#' #' @note This function creates results which may differ in the 3rd decimal
#' #' place from the results from fclimdex.
#' #' @references This function is from the `pacificclimate/climdex.pcic` repository.
#' #' The `climdex.pcic` package was developed and maintained by the Pacific Climate Impacts Consortium (PCIC).
#' #' For more information, visit the repository: <https://github.com/pacificclimate/climdex.pcic>
#' #' Special thanks to the contributors of the `climdex.pcic` package for their efforts in climate data analysis.
#' climdex.dtr <- function(ci, freq=c("monthly", "annual", "seasonal")) { stopifnot(!is.null(ci@data$tmin) && !is.null(ci@data$tmax) && !is.null(ci@data$tavg)); return(compute.mean.daily.temp.range(ci@data$tmax, ci@data$tmin, ci@date.factors[[match.arg(freq)]]) * ci@namasks[[match.arg(freq)]]$tavg) }
#' 
