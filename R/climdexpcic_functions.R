## Class definition declaration
## Check that climdexInput data structure is valid.
valid.climdexInput <- function(object) {
  temp.quantiles <- c(10, 90)
  prec.quantiles <- c(95, 99)
  errors <- c()
  
  separate.base <- c(tmax=T, tmin=T, tavg=T, prec=F)
  present.data.vars <- names(object@data)
  length.check.slots <- c("dates", "jdays")
  length.check.members <- c("date.factors", "data")
  data.lengths <- c(sapply(object@data, length), sapply(length.check.slots, function(y) length(methods::slot(object, y))), unlist(sapply(length.check.members, function(y) { sapply(methods::slot(object, y), length) })))
  quantiles <- list(tmax=temp.quantiles, tmin=temp.quantiles, prec=prec.quantiles)
  
  if(!all(data.lengths == max(data.lengths)))
    errors <- c(errors, "Data fields, dates, and date factors must all be of the same length")
  
  ## Check that namasks have columns for each of the variables
  if(!all(c("annual", "monthly", "seasonal") %in% names(object@namasks)) || !all(present.data.vars %in% names(object@namasks$annual) & present.data.vars %in% names(object@namasks$monthly)& present.data.vars %in% names(object@namasks$seasonal)))
    errors <- c(errors, "NA mask for monthly, seasonal and annual must contain data for all variables supplied.")
  
  ## Check that appropriate thresholds are present.
  need.base.data <- get.num.days.in.range(object@dates, object@base.range) > 0
  
  if(length(object@northern.hemisphere) != 1)
    errors <- c(errors, "northern.hemisphere must be of length 1.")
  
  if(length(errors) == 0)
    return(TRUE)
  else
    return(errors)
}
#' @title climdexInput
#' 
#' @description
#' The climdexInput class contains all the data necessary to compute the
#' climdex indices.
#' 
#' @details
#' The \code{climdexInput} class consists of all the data necessary to compute
#' the climdex indices. Users will not need to modify any of the slots in this
#' class. That being said, users may want or need to repurpose this data for
#' further analysis. The following description of the data is aimed at that
#' audience.
#' 
#' The \code{data} slot contains time series' of daily data of equal length for
#' each of the provided variables. Missing days have been replaced with NA.
#' The \code{dates} slot is the corresponding series of dates (of type PCICt)
#' for the daily data.
#' 
#' The \code{quantiles} slot contains quantiles used for computing the
#' tn/tx 10/90p indices, w/csdi, r95ptot, and r99ptot. If precipitation data
#' is supplied, the 'prec' member contains the 95th and 99th percentile values
#' for precipitation within the base period. For tmin and tmax, if present each
#' will have a corresponding member in the slot. Within each of these, there
#' will be an 'inbase' and 'outbase' member, corresponding to thresholds to be
#' used within the base period (inbase) and outside the base period (outbase).
#' The 'inbase' member consists of one percentile for each day of the year,
#' computed using an n-day (default is 5-day) running window surrounding that
#' day. These percentiles are computed for at least the 10th and 90th
#' percentile of the data. For the 'outbase' member, given n years
#' of data to use as the base period, there are n * (n - 1) sets of daily
#' quantiles of the same type as those in 'inbase'.
#' 
#' To ease computation of monthly, seasonal and annual data, \code{date.factors} 
#' contains date factors which group data into annual, seasonal and monthly time
#' buckets. They are of the same length as the time series and can be reused
#' for computation of any annual, seasonal or monthly aggregates.
#' 
#' The climdexInput class also includes NA masks for seasonal, monthly
#' and annual as parts of the \code{namasks} slot. Each of these masks consist
#' of a vector of numbers of the same length as the monthly, seasonal or annual output
#' data. The values used are 1 to signify that the data meets the QC criteria,
#' and NA to signify it does not. Years with more than (by default) 15 days
#' missing, months with more than (by default) 3 days missing, 
#' and seasons with more than (by default) 6 days missing are
#' considered to be of poor quality and are masked here with NA. These
#' thresholds can be set when instantiating the object, and are stored in the
#' \code{max.missing.days} slot.
#' 
#' Seasons are considered valid only when all three constituent months are valid,
#' and the sum of NA days for the season is no greater than the max.missing.days
#' seasonal argument. This means that a time series starting in January and ending
#' in December will have NA winter seasons at both ends of the series.
#'
#' Seasons are defined as the meteorological seasons:
#'
#' - 'winter': December, January, February
#' - 'spring': March, April, May
#' - 'summer': June, July, August
#' - 'autumn': September, October, November
#' 
#' The \code{base.range} slot contains vector of type PCICt containing the
#' first and last day included in the baseline.
#' 
#' The \code{northern.hemisphere} slot contains a boolean indicating whether
#' the data came from the northern hemisphere. If FALSE, data is assumed to
#' have come from the southern hemisphere. This is used when computing growing
#' season length; if the data is from the southern hemisphere, growing season
#' length is the growing season starting in the beginning of July of the year
#' indicated, running to the end of June of the following year.
#' 
#' The \code{max.missing.days} slot is a vector consisting of 'annual'
#' (the number of days that can be missing in a year), 'monthly' (the
#' number of days that can be missing in a month and 'seasonal' (the
#' number of days that can be missing in a season. If one month in a year fails
#' the test, the corresponding year will be omitted.
#' 
#' @name climdexInput
#' @aliases climdexInput-class
#' @docType class
#' @section Slots: \describe{
#' \item{data}{Time series of supplied data variables.}
#' \item{quantiles}{Threshold quantiles used for threshold-based indices.}
#' \item{namasks}{Data quality masks for annual, seasonal and monthly data.}
#' \item{dates}{Date sequence (type PCICt) corresponding to temperature and
#' precipitation data.}
#' \item{jdays}{Julian days for the date sequence.}
#' \item{base.range}{Date range (type PCICt) of baseline period.}
#' \item{date.factors}{Factors used for creation of annual, seasonal and monthly indices.}
#' \item{northern.hemisphere}{Boolean used when computing growing season
#' length.}
#' \item{max.missing.days}{Maximum number of missing days of data for annual, seasonal
#' and monthly data.}
#' }
#' @seealso \code{\link{climdexInput.raw}}.
#' @keywords climate ts
#' @examples
#' library(PCICt)
#' 
#' ## Parse the dates into PCICt.
#' tmax.dates <- PCICt::as.PCICt(do.call(paste, ec.1018935.tmax[,c("year",
#' "jday")]), format="%Y %j", cal="gregorian")
#' tmin.dates <- PCICt::as.PCICt(do.call(paste, ec.1018935.tmin[,c("year",
#' "jday")]), format="%Y %j", cal="gregorian")
#' prec.dates <- PCICt::as.PCICt(do.call(paste, ec.1018935.prec[,c("year",
#' "jday")]), format="%Y %j", cal="gregorian")
#' 
#' ## Load the data in.
#' ci <- climdexInput.raw(ec.1018935.tmax$MAX_TEMP,
#' ec.1018935.tmin$MIN_TEMP, ec.1018935.prec$ONE_DAY_PRECIPITATION,
#' tmax.dates, tmin.dates, prec.dates, base.range=c(1971, 2000))
#' 
#' @export
setClass("climdexInput",
         representation(data = "list",
                        quantiles = "environment",
                        namasks = "list",
                        dates = "PCICt",
                        jdays = "numeric",
                        base.range = "PCICt",
                        date.factors = "list",
                        northern.hemisphere = "logical",
                        max.missing.days = "numeric"),
         validity=valid.climdexInput
)

#' @title Method for creating climdexInput object from vectors of data
#' 
#' @description
#' This function creates a climdexInput object from data already ingested into
#' R.
#'
#' @param tmax numeric vector containing the data on which the indices are to be computed. (degrees C)
#' @param tmin numeric vector containing the data on which the indices are to be computed. (degrees C)
#' @param prec numeric vector containing the data on which the indices are to be computed. (mm/day)
#' @param tmax.dates vector of type \code{PCICt} of the maximum date.
#' @param tmin.dates vector of type \code{PCICt} of the minimum date.
#' @param prec.dates vector of type \code{PCICt} of the precipitation date.
#' @param base.range a pair of 4 digit years which bound the data on which the base percentiles are calculated.
#' @param n the size of the window used when computing the percentiles used in \code{\link{climdex.tx10p}},
#' \code{\link{climdex.tn10p}}, \code{\link{climdex.tx90p}}, and \code{\link{climdex.tn90p}}.
#' @param northern.hemisphere whether the data came from
#' the northern hemisphere. If FALSE, data is assumed to have come from the
#' southern hemisphere. This is used when computing growing season length; if
#' the data is from the southern hemisphere, growing season length is the
#' growing season starting in the beginning of July of the year indicated,
#' running to the end of June of the following year.
#' @param tavg Average temperature, default `NULL`
#' @param tavg.dates Average temperature dates, default `NULL`
#' @param quantiles supply pre-computed quantiles. This is a list consisting of quantiles for each variable.
#' @param temp.qtiles modify the quantiles calculated. For example, specifying
#' temp.qtiles=c(0.10, 0.50, 0.90) would calculate the 10th, 50th, and 90th
#' percentiles for temperature.
#' @param prec.qtiles modify the quantiles calculated.
#' @param max.missing.days vector consisting of 'annual'
#' (the number of days that can be missing in a year), 'monthly' (the
#' number of days that can be missing in a month and 'seasonal' (the
#' number of days that can be missing in a season. If one month in a year fails
#' the test, the corresponding year will be omitted.
#' @param min.base.fraction.present the minimum fraction
#' of data which must be present for a quantile to be calculated for a 
#' particular day. If the fraction of data present is less than this threshold, 
#' the quantile for that day will be set to NA.
#' @param min.base.data.fraction.present TODO
#' 
#' @details
#' This function takes input climate data at daily resolution, and produces as
#' output a ClimdexInput data structure. This data structure can then be passed
#' to any of the routines used to compute the Climdex indices. The indices
#' themselves are specified on the webpage cited in the references section.
#' 
#' For each temperature variable, there are separate lists of quantiles for 
#' inbase and outbase, with these names. In both cases, quantiles within these
#' lists are named q10 for the 10th percentile and q90 for the 90th percentile.
#' Other percentiles would be named qnn for the nnth percentile. For the
#' outbase quantiles, each element in the list is a vector of length 365 (or 360
#' in the case of 360-day calendars), corresponding to one value for each day of
#' the year. For the inbase quantiles, each element in the list is an array of
#' dimensions \code{[365 or 360, nyr, nyr - 1]}, where nyr is the number of years in
#' the base period. Each value corresponds to a quantile for each day, for each
#' year, with a particular year replaced.
#'
#' For precipitation variables, there is a named vector of quantiles, consisting
#' of at least q95 and q99. 
#' 
#' @keywords ts climate
#' @references For Climdex Indices: \url{http://etccdi.pacificclimate.org/list_27_indices.shtml}
#' This function is from the `pacificclimate/climdex.pcic` repository. 
#' The `climdex.pcic` package was developed and maintained by the Pacific Climate Impacts Consortium (PCIC).
#' For more information, visit the repository: <https://github.com/pacificclimate/climdex.pcic>
#' Special thanks to the contributors of the `climdex.pcic` package for their efforts in climate data analysis.
#'
#' @return An object of class \code{\link{climdexInput-class}} for use with
#' other climdex methods.
#' 
#' @note Units are assumed to be mm/day for precipitation and degrees Celsius
#' for temperature. No units conversion is performed internally.
#' 
#' @examples
#' library(PCICt)
#'
#' ## Create a climdexInput object from some data already loaded in and
#' ## ready to go.
#' 
#' ## Parse the dates into PCICt.
#' #tmax.dates <- PCICt::as.PCICt(do.call(paste, ec.1018935.tmax[,c("year",
#' #"jday")]), format="%Y %j", cal="gregorian")
#' #tmin.dates <- PCICt::as.PCICt(do.call(paste, ec.1018935.tmin[,c("year",
#' #"jday")]), format="%Y %j", cal="gregorian")
#' #prec.dates <- PCICt::as.PCICt(do.call(paste, ec.1018935.prec[,c("year",
#' #"jday")]), format="%Y %j", cal="gregorian")
#' 
#' ## Load the data in.
#' #ci <- climdexInput.raw(ec.1018935.tmax$MAX_TEMP,
#' #ec.1018935.tmin$MIN_TEMP, ec.1018935.prec$ONE_DAY_PRECIPITATION,
#' #tmax.dates, tmin.dates, prec.dates, base.range=c(1971, 2000))
#' @export 
climdexInput.raw <- function(tmax=NULL, tmin=NULL, prec=NULL, tmax.dates=NULL, tmin.dates=NULL, prec.dates=NULL,
                             base.range=c(1961, 1990), n=5, northern.hemisphere=TRUE,
                             tavg=NULL, tavg.dates=NULL, quantiles=NULL, temp.qtiles=c(0.10, 0.90), prec.qtiles=c(0.95, 0.99), max.missing.days=c(annual=15, monthly=3, seasonal=6), min.base.data.fraction.present=0.1) {
  #stopifnot(length(max.missing.days) == 3 && all(c("annual", "monthly", "seasonal") %in% names(max.missing.days)))
  #stopifnot(is.numeric(min.base.data.fraction.present) && length(min.base.data.fraction.present) == 1)
  
  d.list <- list(tmin.dates, tmax.dates, prec.dates, tavg.dates)
  all.dates <- do.call(c, d.list[!sapply(d.list, is.null)])
  last.day.of.year <- get.last.monthday.of.year(all.dates)
  cal <- attr(all.dates, "cal")
  
  ## Convert base range (in years) to PCICt
  bs.date.range <- PCICt::as.PCICt(paste(base.range, c("01-01", last.day.of.year), sep="-"), cal=cal)
  bs.date.series <- seq(bs.date.range[1], bs.date.range[2], by="day")
  
  ## Get dates for normal data
  new.date.range <- PCICt::as.PCICt(paste(as.numeric(format(range(all.dates), "%Y", tz="GMT")), c("01-01", last.day.of.year), sep="-"), cal=cal)
  date.series <- seq(new.date.range[1], new.date.range[2], by="day")
  jdays <- get.jdays.replaced.feb29(get.jdays(date.series))
  
  # Classify meteorological seasons with associated years. This includes 
  # handling for the winter season, where data in the months of January and 
  # February are assigned to the winter season of the previous year.
  # 
  # Seasons are defined as the meteorological seasons:
  #  - 'Winter': December, January, February
  #  - 'Spring': March, April, May
  #  - 'Summer': June, July, August
  #  - 'Fall': September, October, November
  classify_meteorological_season_with_year <- function(date.series) {
    month <- as.integer(format(date.series, "%m"))
    year <- as.integer(format(date.series, format = "%Y"))
    year_for_season <- ifelse(month %in% c(1, 2), year - 1, year)
    
    # Vector combining season and year
    season_with_year <- ifelse(month %in% c(12, 1, 2), paste("Winter", year_for_season),
                               ifelse(month %in% 3:5, paste("Spring", year_for_season),
                                      ifelse(month %in% 6:8, paste("Summer", year_for_season),
                                             ifelse(month %in% 9:11, paste("Fall", year_for_season), NA)
                                      )
                               )
    )
    return(season_with_year)
  }
  season_with_year <- classify_meteorological_season_with_year(date.series)
  
  ## Factors for dividing data up
  date.factors <- list(
    annual = factor(format(date.series, format = "%Y", tz = "GMT")),
    monthly = factor(format(date.series, format = "%Y-%m", tz = "GMT")),
    seasonal = factor(season_with_year, levels = unique(season_with_year))
  )
  ## Filled data...
  var.list <- c("tmax", "tmin", "prec", "tavg")
  present.var.list <- var.list[sapply(var.list, function(x) !is.null(get(x)))]
  filled.list <- sapply(present.var.list, function(x) { return(create.filled.series(get(x), trunc(get(paste(x, "dates", sep="."))), date.series)) }, simplify=FALSE)
  if(is.null(tavg) && !is.null(tmin) && !is.null(tmax))
    filled.list$tavg <- (filled.list$tmax + filled.list$tmin) / 2
  
  ## Establish some truth values for later use in logic...
  days.threshold <- 359
  present.dates <- sapply(present.var.list, function(x) get(paste(x, "dates", sep=".")))
  quantile.dates <- list(tmax=tmax.dates, tmin=tmin.dates, prec=prec.dates)
  days.in.base <- sapply(quantile.dates, get.num.days.in.range, bs.date.range)
  
  ## Check that provided quantiles, if any, are valid
  check.quantile.validity(quantiles, present.var.list, days.in.base)
  
  data.in.base.period <- any(days.in.base != 0)
  have.quantiles <- all(present.var.list %in% names(quantiles))
  
  ## NA masks
  namasks <- list(
    annual = lapply(filled.list, get.na.mask, date.factors$annual, max.missing.days["annual"]),
    monthly = lapply(filled.list, get.na.mask, date.factors$monthly, max.missing.days["monthly"]),
    seasonal = lapply(filled.list, get.na.mask, date.factors$seasonal, max.missing.days["seasonal"]))
  namasks$annual <- lapply(names(namasks$annual), function(v) {
    d <- namasks$annual[[v]] * as.numeric(tapply(namasks$monthly[[v]], rep(seq_along(namasks$annual[[v]]), each = 12), prod))
    dimnames(d) <- dim(d) <- NULL
    d
  })
  names(namasks$annual) <- names(namasks$seasonal) <- names(namasks$monthly)
  
  
  season_month_counts <- sapply(unique(date.factors$seasonal), function(season) {
    length(unique(date.factors$monthly[date.factors$seasonal == season]))
  })
  data.vars <- present.var.list
  if (!is.null(namasks$seasonal$tavg)){
    data.vars <- c(data.vars, "tavg")
  }
  for (var in data.vars) {
    seasonal_namasks <- namasks$seasonal[[var]]
    na_months <- unique(date.factors$monthly)[is.na(namasks$monthly[[var]])]
    seasons_of_na_months <- unique(date.factors$seasonal[date.factors$monthly %in% na_months])
    seasonal_namasks[unique(date.factors$seasonal) %in% seasons_of_na_months] <- NA
    # Identify and set NA for seasons with less than 3 months
    for (season in seq_along(season_month_counts) ) {
      if (!is.na(season_month_counts[season]) && season_month_counts[season] < 3) {
        seasonal_namasks[season] <- NA
      }
    }
    namasks$seasonal[[var]] <- seasonal_namasks
  } 
  
  
  ## Pad data passed as base if we're missing endpoints...
  if(!have.quantiles) {
    quantiles <- new.env(parent=emptyenv())
    
    if(days.in.base['tmax'] > days.threshold)
      delayedAssign("tmax", get.temp.var.quantiles(filled.list$tmax, date.series, bs.date.series, temp.qtiles, bs.date.range, n, TRUE, min.base.data.fraction.present), assign.env=quantiles)
    if(days.in.base['tmin'] > days.threshold)
      delayedAssign("tmin", get.temp.var.quantiles(filled.list$tmin, date.series, bs.date.series, temp.qtiles, bs.date.range, n, TRUE, min.base.data.fraction.present), assign.env=quantiles)
    if(days.in.base['prec'] > days.threshold)
      delayedAssign("prec", get.prec.var.quantiles(filled.list$prec, date.series, bs.date.range, prec.qtiles), assign.env=quantiles)
  } else {
    quantiles <- as.environment(quantiles)
  }
  
  return(methods::new("climdexInput", data=filled.list, quantiles=quantiles, namasks=namasks, dates=date.series, jdays=jdays, base.range=bs.date.range, date.factors=date.factors, northern.hemisphere=northern.hemisphere, max.missing.days=max.missing.days))
}

## Get the last month and day of the year as a character sting, separated by the specified separator.
## This is a utility function necessitated by 360-day calendars. Works on PCICt objects.
get.last.monthday.of.year <- function(d, sep="-") {
  if(!is.null(attr(d, "months"))) paste("12", attr(d, "months")[12], sep=sep) else paste("12", "31", sep=sep)
}

## Get julian day of year
get.jdays <- function(dates) {
  return(as.POSIXlt(dates)$yday + 1)
}

## Get year
get.years <- function(dates) {
  return(as.POSIXlt(dates)$year + 1900)
}

## Get month number
get.months <- function(dates) {
  return(as.POSIXlt(dates)$mon + 1)
}

## Juggle the list so that day 366 == day 365
get.jdays.replaced.feb29 <- function(jdays) {
  indices <- which(jdays == 366)
  if(length(indices) > 0)
    jdays[rep(indices, each=366) + -365:0] <- c(1:59, 59, 60:365)
  jdays
}

## Converts a positional index with respect to some origin into a PCICt object in the format %Y-%m-%d.
ymd.dates <- function(origin, cal, exact.day, val) {
  origin.pcict <- PCICt::as.PCICt(origin, cal)
  seconds.per.day <- 86400
  exact.day.pcict <- origin.pcict + (ifelse(is.na(exact.day),1,exact.day - 1)) * seconds.per.day
  ymd <- PCICt::as.PCICt(exact.day.pcict, cal = cal, format = "%Y-%m-%d")
  ymd <- format(ymd, "%Y-%m-%d")
  ymd[is.na(val)] <- NA
  return(ymd)
}

## Computes exact dates for statistics based on the specified frequency (annual, monthly, or seasonal).
exact.date <- function(stat, data, date.factor, freq, cal, mask) {
  val <- suppressWarnings(tapply.fast(data, date.factor, get(stat), na.rm = TRUE)) * mask
  exact.day <- suppressWarnings(tapply.fast(data, date.factor, get(paste("which.", stat, sep = "")))) 
  df <- data.frame(
    val = val,
    ymd = {
      origin <- sapply(1:length(unique(date.factor)), function(i) {
        switch(
          as.character(freq),
          annual = paste((unique(date.factor))[[i]], "01-01", sep = "-"),
          monthly = paste((unique(date.factor))[[i]], "01", sep = "-"),
          seasonal = {
            season.year <- strsplit(as.character(unique(date.factor)[[i]]), " ")
            year <- as.numeric(season.year[[1]][2])
            season <- season.year[[1]][1]
            season.months <- list(Winter = "12", Spring = "03", Summer = "06", Fall = "09")
            paste(year, season.months[[season]], "01", sep = "-")
          }
        )
      })
      ymd.dates(origin, cal, exact.day, val)
    }
  )
  return(df)
}

## Check validity of quantile input.
check.quantile.validity <- function(quantiles, present.vars, days.in.base) {
  if(is.null(quantiles))
    return()
  
  if(!inherits(quantiles, "list"))
    stop("Provided quantiles must be a list.")
  
  if(!all(present.vars %in% names(quantiles)))
    stop("Quantiles must be present for all variables provided.\n")
  
  if(!all(sapply(quantiles[names(quantiles) %in% intersect(present.vars, c("tmax", "tmin"))], function(x) { "outbase" %in% names(x) && all(c("q10", "q90") %in% names(x$outbase)) })))
    stop("Temperature out-of-base quantiles must contain 10th and 90th percentiles.\n")
  
  if(any(days.in.base > 0) && !all(sapply(quantiles[names(quantiles) %in% intersect(intersect(present.vars, c("tmax", "tmin")), names(days.in.base)[days.in.base > 0])], function(x) { "inbase" %in% names(x) && all(c("q10", "q90") %in% names(x$inbase)) })))
    stop("Temperature in-base quantiles must contain 10th and 90th percentiles.\n")
  
  if("prec" %in% names(quantiles) && !all(c("q95", "q99") %in% names(quantiles$prec)))
    stop("Precipitation quantiles must contain 95th and 99th percentiles.\n")
}

## Creates a filled series given the data, dates, and new date sequence to be used.
create.filled.series <- function(data, data.dates, new.date.sequence) {
  new.data <- rep(NA, length(new.date.sequence))
  data.in.new.data <- (data.dates >= new.date.sequence[1]) & (data.dates <= new.date.sequence[length(new.date.sequence)])
  indices <- floor(as.numeric(data.dates[data.in.new.data] - new.date.sequence[1], units="days")) + 1
  new.data[indices] <- data[data.in.new.data]
  return(new.data)
}

## Get NA mask given threshold and split factor
get.na.mask <- function(x, f, threshold) {
  return(c(1, NA)[1 + as.numeric(tapply.fast(is.na(x), f, function(y) { return(sum(y) > threshold) } ))])
}

## Get set of days for bootstrap use
get.bootstrap.set <- function(dates, bootstrap.range, win.size) {
  dpy <- ifelse(is.null(attr(dates, "dpy")), 365, attr(dates, "dpy"))
  return(dates >= bootstrap.range[1] & dates <= bootstrap.range[2] & (dpy == 360 | format(dates, format="%m-%d", tz="GMT") != "02-29"))
}

## Get series length at ends
get.series.lengths.at.ends <- function(x, na.value=FALSE) {
  stopifnot(is.logical(x) && is.logical(na.value))
  n <- length(x)
  if(n == 1)
    return(as.numeric(x))
  
  res <- rep(0, n)
  x[is.na(x)] <- na.value
  
  ## Compare series to lag-1 and lag+1 series; false added to trigger state transition from TRUE at ends of series
  start <- which(x & !(c(FALSE, x[1:(n - 1)])))
  end <- which(x & !(c(x[2:n], FALSE)))
  res[end] <- end - start + 1
  return(res)
}

## Select blocks of TRUE values of sufficient length.
select.blocks.gt.length <- function(d, n, na.value=FALSE) {
  stopifnot(is.logical(d), is.numeric(n))
  
  if(n < 1)
    return(d)
  
  if(n >= length(d))
    return(rep(FALSE, length(d)))
  
  d[is.na(d)] <- na.value
  
  d2 <- Reduce(function(x, y) { return(c(rep(FALSE, y), d[1:(length(d) - y)]) & x) }, 1:n, d)
  return(Reduce(function(x, y) { return(c(d2[(y + 1):length(d2)], rep(FALSE, y)) | x) }, 1:n, d2))
}

## Lower overhead version of tapply
tapply.fast <- function(X, INDEX, FUN = NULL, ..., simplify = TRUE) {
  FUN <- if (!is.null(FUN)) {
    match.fun(FUN)
  }
  
  if (!is.factor(INDEX)) {
    stop("INDEX must be a factor.")
  }
  
  if (length(INDEX) != length(X)) {
    stop("arguments must have the same length")
  }
  
  if (is.null(FUN)) {
    return(INDEX)
  }
  
  
  ans <- lapply(split(X, INDEX), FUN, ...)
  if (is.function(FUN) && (identical(FUN, which.min) || identical(FUN, which.max))) {
    # Handle which.min & which.max separately
    ans <- lapply(ans, function(x) if (length(x) == 0) NA else x)
    ans <- unlist(ans)
  } else {
    ans <- unlist(ans, recursive = FALSE)
  }
  names(ans) <- levels(INDEX)
  
  return(ans)
}

## Calculate a running quantile on the data set over the bootstrap range.
## If get.bootstrap.data is TRUE, use the Zhang boostrapping method described in Xuebin Zhang et al's 2005 paper, "Avoiding Inhomogeneity in Percentile-Based Indices of Temperature Extremes" J.Clim vol 18 pp.1647-1648, "Removing the 'jump'".
## Expects PCICt for all dates
zhang.running.qtile <- function(x, dates.base, qtiles, bootstrap.range, include.mask=NULL, n=5, get.bootstrap.data=FALSE, min.fraction.present=0.1) {
  inset <- get.bootstrap.set(dates.base, bootstrap.range, n)
  dpy <- ifelse(is.null(attr(dates.base, "dpy")), 365, attr(dates.base, "dpy"))
  nyears <- floor(sum(inset) / dpy)
  
  if (!is.null(include.mask))
    x[include.mask] <- NA
  
  bs.data <- x[inset]
  
  qdat <- NULL
  if (get.bootstrap.data) {
    d <- running_quantile_windowed_bootstrap_R(bs.data, n, qtiles, dpy, min.fraction.present, n_bootstrap = nyears - 1)
    dim(d) <- c(dpy, nyears, nyears - 1, length(qtiles))
    qdat <- lapply(1:length(qtiles), function(i) { 
      r <- d[,,,i, drop=FALSE]
      dim(r) <- dim(r)[1:3]
      r
    })
  } else {
    res <- running.quantile(bs.data, n, qtiles, dpy, min.fraction.present)
    qdat <- lapply(1:length(qtiles), function(i) res[,i])
  }
  
  names(qdat) <- paste("q", qtiles * 100, sep="")
  return(qdat)
}

get.temp.var.quantiles <- function(filled.data, date.series, bs.date.series, qtiles, bs.date.range, n, in.base=FALSE, min.base.data.fraction.present=0.1) {
  base.data <- create.filled.series(filled.data, date.series, bs.date.series)
  if(in.base)
    return(list(outbase=zhang.running.qtile(base.data, dates.base=bs.date.series, qtiles=qtiles, bootstrap.range=bs.date.range, n=n, min.fraction.present=min.base.data.fraction.present),
                inbase=zhang.running.qtile(base.data, dates.base=bs.date.series, qtiles=qtiles, bootstrap.range=bs.date.range, n=n, get.bootstrap.data=TRUE, min.fraction.present=min.base.data.fraction.present)))
  else
    return(list(outbase=zhang.running.qtile(base.data, dates.base=bs.date.series, qtiles=qtiles, bootstrap.range=bs.date.range, n=n, min.fraction.present=min.base.data.fraction.present)))
}

get.prec.var.quantiles <- function(filled.prec, date.series, bs.date.range, qtiles=c(0.95, 0.99)) {
  wet.days <- !(is.na(filled.prec) | filled.prec < 1)
  inset <- date.series >= bs.date.range[1] & date.series <= bs.date.range[2] & !is.na(filled.prec) & wet.days
  pq <- stats::quantile(filled.prec[inset], qtiles, type=8)
  names(pq) <- paste("q", qtiles * 100, sep="")
  return(pq)
}

## Computes a specified statistic (min, max) for a given climate index and frequency.
compute.stat <- function(ci, stat, data.key, freq = c("monthly", "annual", "seasonal"), include.exact.dates) {
  stopifnot(!is.null(ci@data[[data.key]]))
  data <- ci@data[[data.key]]
  date.factors <- ci@date.factors[[match.arg(freq)]]
  mask <- ci@namasks[[match.arg(freq)]][[data.key]]
  cal <- attr(ci@dates, "cal")
  
  if (include.exact.dates) {
    return(exact.date(stat, data, date.factors, freq, cal, mask))
  }
  
  return(suppressWarnings(tapply.fast(data, date.factors, stat, na.rm = TRUE)) * mask)
}

## Returns an n-day running quantile for each day of data (dimensions c(dpy, q))
running.quantile <- function(data, n, q, dpy, min.fraction) {
  ret <- running_quantile_windowed_R(data, n, q, dpy, min.fraction)
  dim(ret) <- c(length(q), dpy)
  return(t(ret))
}

# Running quantile computation using only R
running_quantile_windowed_R <- function(data, n, q, dpy, min.fraction) {
  ndays <- length(data)
  ret <- matrix(NA, nrow = length(q), ncol = dpy)
  
  for (i in 1:dpy) {
    indices <- seq(i, ndays, by = dpy)  # Get indices corresponding to the same day across years
    values <- unlist(lapply(indices, function(idx) {
      range <- max(1, idx - floor(n/2)) : min(ndays, idx + floor(n/2))
      data[range]
    }))
    values <- values[!is.na(values)]  # Remove missing values
    
    if (length(values) / length(indices) >= min.fraction) {
      ret[, i] <- stats::quantile(values, probs = q, na.rm = TRUE)
    }
  }
  
  return(ret)
}

running_quantile_windowed_bootstrap_R <- function(data, n, qtiles, dpy, min.fraction.present, n_bootstrap) {
  nyears <- floor(length(data) / dpy)
  result <- array(NA, dim = c(dpy, nyears, n_bootstrap, length(qtiles)))
  
  for (b in 1:n_bootstrap) {
    sampled_years <- sample(1:nyears, nyears, replace = TRUE)
    for (i in 1:dpy) {
      window_values <- unlist(lapply(sampled_years, function(y) {
        index <- (y - 1) * dpy + (max(1, i - floor(n/2)) : min(dpy, i + floor(n/2)))
        data[index]
      }))
      window_values <- window_values[!is.na(window_values)]
      
      if (length(window_values) / length(sampled_years) >= min.fraction.present) {
        result[i, , b, ] <- stats::quantile(window_values, probs = qtiles, na.rm = TRUE)
      }
    }
  }
  
  return(result)
}


## Get number of days within range
get.num.days.in.range <- function(x, date.range) {
  return(sum(x >= date.range[1] & x <= date.range[2]))  
}

## Helper function to extract parameters for rxnday indices.
get.rxnday.params <- function(ci, freq= c("monthly", "annual", "seasonal")) {
  stopifnot(!is.null(ci@data$prec))
  data <- ci@data$prec
  date.factors <- ci@date.factors[[match.arg(freq)]]
  mask <- ci@namasks[[match.arg(freq)]]$prec
  cal <- attr(ci@dates, "cal")
  return(list(data = data, date.factor = date.factors, mask = mask, cal = cal))
}

## Number of days (less than, greater than, etc) a threshold
number.days.op.threshold <- function(temp, date.factor, threshold, op="<") {
  stopifnot(is.numeric(temp) && is.numeric(threshold) && is.factor(date.factor))
  return(tapply.fast(match.fun(op)(temp, threshold), date.factor, sum, na.rm=TRUE))
}

## Lengths of strings of TRUE values
percent.days.op.threshold <- function(temp, dates, jdays, date.factor, threshold.outside.base, base.thresholds, base.range, op='<', max.missing.days) {
  f <- match.fun(op)
  dat <- f(temp, threshold.outside.base[jdays])
  
  inset <- dates >= base.range[1] & dates <= base.range[2]
  ## Don't use in-base thresholds with data shorter than two years; no years to replace with.
  if(sum(inset) > 0 && length(dates) >= 360 * 2) {
    jdays.base <- jdays[inset]
    years.base <- get.years(dates[inset])
    
    ## Get number of base years, subset temp data to base period only.
    temp.base <- temp[inset]
    years.base.range <- range(years.base)
    byrs <- (years.base.range[2] - years.base.range[1] + 1)
    
    ## Linearize thresholds, then compare them to the temperatures
    bdim <- dim(base.thresholds)
    dim(base.thresholds) <- c(bdim[1] * bdim[2], bdim[3])
    yday.byr.indices <- jdays.base + (years.base - get.years(base.range)[1]) * bdim[1]
    f.result <- f(rep(temp.base, byrs - 1), base.thresholds[yday.byr.indices,])
    dim(f.result) <- c(length(yday.byr.indices), bdim[3])
    
    ## Chop up data along the 2nd dim into a list; sum elements of the list
    dat[inset] <- rowSums(f.result, na.rm=TRUE) / (byrs - 1)
  }
  dat[is.nan(dat)] <- NA
  if(missing(date.factor))
    return(dat)
  na.mask <- get.na.mask(dat, date.factor, max.missing.days)
  ## FIXME: Need to monthly-ize the NA mask calculation, which will be ugly.
  ret <- tapply.fast(dat, date.factor, mean, na.rm=TRUE) * 100 * na.mask
  ret[is.nan(ret)] <- NA
  return(ret)
}

## @title Sum of spell lengths exceeding daily threshold
threshold.exceedance.duration.index <- function(daily.temp, date.factor, jdays, thresholds, op=">", min.length=6, spells.can.span.years=TRUE, max.missing.days) {
  stopifnot(is.numeric(c(daily.temp, thresholds, min.length)), is.factor(date.factor),
            is.function(match.fun(op)),
            min.length > 0)
  f <- match.fun(op)
  na.mask <- get.na.mask(is.na(daily.temp + thresholds[jdays]), date.factor, max.missing.days)
  
  if(spells.can.span.years) {
    periods <- select.blocks.gt.length(f(daily.temp, thresholds[jdays]), min.length - 1)
    return(tapply.fast(periods, date.factor, sum) * na.mask)
  } else {
    ## fclimdex behaviour...
    return(tapply.fast(1:length(daily.temp), date.factor, function(idx) { sum(select.blocks.gt.length(f(daily.temp[idx], thresholds[jdays[idx]]), min.length - 1)) } ) * na.mask)
  }
}

## @title Number of days (less than, greater than, etc) a threshold
nday.consec.prec.max <- function(daily.prec, date.factor, ndays, center.mean.on.last.day = FALSE, include.exact.dates = FALSE, mask = 1, freq, cal) {
  stat <- "max"
  if (ndays == 1) {
    if (include.exact.dates) {
      
      df <- exact.date(stat, daily.prec, date.factor, freq, cal, mask)
      return(df)
    }
    return(suppressWarnings(tapply.fast(daily.prec, date.factor, stat, na.rm = TRUE)) * mask)
  }
  ## Ends of the data will be de-emphasized (padded with zero precip data); NAs replaced with 0
  daily.prec[is.na(daily.prec)] <- 0
  prec.runsum <- running.mean(daily.prec, ndays)
  prec.runsum[is.na(prec.runsum)] <- 0
  
  if (center.mean.on.last.day) {
    k2 <- ndays %/% 2
    prec.runsum <- c(rep(0, k2), prec.runsum[1:(length(prec.runsum) - k2)])
  }
  if (include.exact.dates) {
    df <- exact.date(stat, prec.runsum, date.factor, freq, cal, mask)
    df$val <- df$val * ndays
    return(df)
  }
  return((tapply.fast(prec.runsum, date.factor, stat) * ndays) * mask)
}

## @title Maximum spell length
spell.length.max <- function(daily.prec, date.factor, threshold, op, spells.can.span.years, include.exact.dates = FALSE, mask = 1, cal= "365") {
  bools <- match.fun(op)(daily.prec, threshold)
  spells <- get.series.lengths.at.ends(bools)
  if (spells.can.span.years) {
    all.true <- tapply.fast(bools, date.factor, all)
    max.spell <- tapply.fast(spells, date.factor, max)
    
    ## Mask out values which are in the middle of a spell with NA
    na.mask <- c(1, NA)[as.integer((max.spell == 0) & all.true) + 1]
    max.spell <- max.spell * na.mask
  } else {
    max.spell <- tapply.fast(bools, date.factor, function(x) {
      max(get.series.lengths.at.ends(x))
    })
  }
  if (include.exact.dates) {
    end <- tapply.fast(bools, date.factor, function(x) {
      which.max(get.series.lengths.at.ends(x))
    })
    
    start <- end - max.spell
    origin <- paste(names(max.spell), "01", "01", sep = "-")
    origin.pcict <- PCICt::as.PCICt(origin, cal)
    seconds.per.day <- 86400
    start.pcict <- origin.pcict + start * seconds.per.day
    end.pcict <- origin.pcict + (end - 1) * seconds.per.day
    
    df <- data.frame(
      start = format(start.pcict, "%Y-%m-%d"),
      duration = max.spell * mask,
      end = format(end.pcict, "%Y-%m-%d")
    )
    
    df$start[which(df$duration == 0)]<- NA
    df$end[which(df$duration == 0)] <- NA
    
    df$start[is.na(df$duration)] <- NA
    df$end[is.na(df$duration)] <- NA
    return(df)
  } else {
    return(max.spell * mask)
  }
}

## Sum of precipitation above a threshold
total.precip.op.threshold <- function(daily.prec, date.factor, threshold, op) {
  f <- match.fun(op)
  return(tapply.fast(daily.prec, date.factor, function(pr) { return(sum(pr[f(pr, threshold)], na.rm=TRUE)) } ))
}

## @title Running Mean of a Vector
running.mean <- function(vec, bin){
  vec = as.vector(vec)
  len = length(vec)
  if (bin<=1) {
    return (vec)
  }
  if (bin > len) {
    bin = len
  }
  left.bin = bin%/%2
  
  means = double(len)
  
  right.bin = bin - left.bin - 1
  means = c( sum(vec[1:bin]), diff(vec,bin) ) # find the first sum and the differences from it
  means = cumsum(means)/bin                  # apply precomputed differences
  means = c(rep(NA,left.bin), means, rep(NA,right.bin))   # extend to original vector length
  return(means)
}

## Simple Precipitation Intensity Index
simple.precipitation.intensity.index <- function(daily.prec, date.factor) {
  return(tapply.fast(daily.prec, date.factor, function(prec) { idx <- prec >= 1 & !is.na(prec); if(sum(idx) == 0) { return(0); } else { return(sum(prec[idx], na.rm=TRUE) / sum(idx)) } } ))
}

## DTR
## Computes mean diurnal temperature range in each period (as specified by date.factor).
## Max and min temps are assumed to be same length
compute.mean.daily.temp.range <- function(daily.max.temp, daily.min.temp, date.factor) {
  dat <- tapply.fast(daily.max.temp - daily.min.temp, date.factor, mean, na.rm=TRUE)
  dat[is.nan(dat)] <- NA
  dat
}

## Flexible GSL function - This function computes the growing season length (GSL) given the input,
## which is allowed to vary considerably from the ETCCDI definitions.
growing.season.length <- function(daily.mean.temp, date.factor, dates, northern.hemisphere,
                                  min.length = 6, t.thresh = 5, gsl.mode = c("GSL", "GSL_first", "GSL_max", "GSL_sum"), include.exact.dates = FALSE, cal) {
  gsl.mode <- match.arg(gsl.mode)
  month.series <- get.months(dates)
  transition.month <- if (northern.hemisphere) 7 else 1
  if (gsl.mode == "GSL") {
    growing.season <- (tapply.fast(1:length(daily.mean.temp), date.factor, function(idx) {
      temp.data <- daily.mean.temp[idx]
      ts.mid <- utils::head(which(month.series[idx] == transition.month), n = 1)
      if (!length(ts.mid)) {
        return(NA)
      }
      
      ts.len <- length(temp.data)
      gs.begin <- which(select.blocks.gt.length(temp.data[1:(ts.mid - 1)] > t.thresh, min.length - 1))
      
      ## Growing season actually ends the day -before- the sequence of sketchy days
      gs.end <- which(select.blocks.gt.length(temp.data[ts.mid:ts.len] < t.thresh, min.length - 1)) - 1
      
      ## If no growing season start, 0 length; if no end, ends at end of year; otherwise, end - start + 1
      # season.length <- ifelse(length(gs.begin) == 0, 0, ifelse(length(gs.end) == 0, ts.len - gs.begin[1] + 1, gs.end[1] - gs.begin[1] + ts.mid))
      
      if (is.na(gs.begin[1])) {
        start <- NA_character_
        end <- NA_character_
        season.length <- 0  # Set season length to 0
      } 
      else if (is.na(gs.end[1])) {
        start <- gs.begin[1]
        end <- ts.len - 1 # Last DOY
        season.length <- ts.len - start + 1
      } 
      else {
        start <- gs.begin[1]
        end <- gs.end[1] + ts.mid -1
        season.length <- end - start +1 
      }
      
      if (include.exact.dates) {
        if(northern.hemisphere){
          origin <- paste(year = unique(date.factor[idx]), "01", "01", sep = "-")
        }
        else {origin <- paste(year = unique(date.factor[idx]), "07", "01", sep = "-")}
        
        origin.pcict <- PCICt::as.PCICt(origin, cal)
        seconds.per.day <- 86400
        
        if (!is.na(start)){
          start.pcict <- origin.pcict + (start - 1) * seconds.per.day
          start <- format(start.pcict, "%Y-%m-%d")
          end.pcict <- origin.pcict + (end * seconds.per.day)
          end <- format(end.pcict, "%Y-%m-%d")
        }
        
        result <- c(start, sl <- season.length, end)
      } 
      else { result <- season.length}
      return(result)
    }))
    
    if (include.exact.dates) {
      start <- growing.season[seq(1, length(growing.season), by = 3)]
      sl <- growing.season[seq(2, length(growing.season), by = 3)]
      sl <- as.numeric(sl)
      end <- growing.season[seq(3, length(growing.season), by = 3)]
      year <- names(growing.season[1:length(start)]) 
      if(!northern.hemisphere){
        sl <- c(sl,NA)
        end <- c(end,NA)
      }
      df <- data.frame(start,sl, end)
      rownames(df) <- year
      return(df)
    }
    return(growing.season)
  } 
  else {
    in.gsl <- !select.blocks.gt.length(!select.blocks.gt.length(daily.mean.temp >= t.thresh, min.length - 1), min.length - 1)
    warning("GSL_first, GSL_max, and GSL_sum are experimental alternative growing season length definitions. Use at your own risk.")
    
    innerfunc <- switch(gsl.mode,
                        GSL_first = function(bl) {
                          ifelse(any(bl > 0), (bl[bl > 0])[1], 0)
                        },
                        GSL_max = max,
                        GSL_sum = sum
    )
    return(tapply.fast(in.gsl, date.factor, function(ts) {
      block.lengths <- get.series.lengths.at.ends(ts)
      return(innerfunc(block.lengths))
    }))
  }
}