
#' climdex_single_station
#' @description
#' This function calculates climate indices for a single weather station based on the provided parameters. The function utilizes the climdex.pcic package to compute various climate indices. The indices can be calculated either on an annual or monthly basis, depending on the specified frequency.
#'
#' @param ci
#' The climate data for the weather station.
#'
#' @param freq
#' The frequency at which the climate indices should be calculated. It can be either "annual" or "monthly" (default: "annual").
#'
#' @param indices
#' A character vector specifying the climate indices to be calculated.
#'
#' @param year
#' The column name or index of the year in the climate data.
#'
#' @param month
#' The column name or index of the month in the climate data. Required only if freq = "monthly".
#'
#' @param spells.can.span.years
#' A logical value indicating whether the spells can span multiple years. This parameter is used by certain indices that involve consecutive days, such as "wsdi", "csdi", "cdd", and "cwd" (default: FALSE).
#'
#' @param gsl.mode
#' The mode used to calculate growing season length (GSL) index. It is used by the "gsl" index. Please refer to the documentation of climdex.pcic::climdex.gsl for details.
#'
#' @param threshold
#' A threshold value used by the "rnnmm" index to calculate the number of consecutive wet days above the threshold (default: 1).
#'
#' @return
#' A data frame containing the calculated climate indices for the specified station and time period.
#'
#' @export
#'
#' @examples
#'#data <- read.csv("climate_data.csv")
#'#climdex_single_station(data, freq = "annual", indices = c("fd", "su"),
#'# year = "Year")


climdex_single_station <- function(ci, freq = "annual", indices, year, month,
                                   spells.can.span.years = FALSE, gsl.mode = gsl.mode,
                                   threshold = 1) {
  stopifnot(freq %in% c("annual", "monthly"))
  if (freq == "monthly" && missing(month)) stop("month is required for freq = 'monthly'.")
  if (missing(indices)) stop("No indices specified.")
  for (i in seq_along(indices)) {
    vals <- switch(indices[i],
                   "fd" = climdex.pcic::climdex.fd(ci),
                   "su" = climdex.pcic::climdex.su(ci),
                   "id" = climdex.pcic::climdex.id(ci),
                   "tr" = climdex.pcic::climdex.tr(ci),
                   "wsdi" = climdex.pcic::climdex.wsdi(ci, spells.can.span.years = spells.can.span.years),
                   "csdi" = climdex.pcic::climdex.csdi(ci, spells.can.span.years = spells.can.span.years),
                   "gsl" = climdex.pcic::climdex.gsl(ci, gsl.mode = gsl.mode),
                   "txx" = climdex.pcic::climdex.txx(ci, freq = freq),
                   "txn" = climdex.pcic::climdex.txn(ci, freq = freq),
                   "tnx" = climdex.pcic::climdex.tnx(ci, freq = freq),
                   "tnn" = climdex.pcic::climdex.tnn(ci, freq = freq),
                   "tn10p" = climdex.pcic::climdex.tn10p(ci, freq = freq),
                   "tx10p" = climdex.pcic::climdex.tx10p(ci, freq = freq),
                   "tn90p" = climdex.pcic::climdex.tn90p(ci, freq = freq),
                   "tx90p" = climdex.pcic::climdex.tx90p(ci, freq = freq),
                   "dtr" = climdex.pcic::climdex.dtr(ci, freq = freq),
                   "rx1day" = climdex.pcic::climdex.rx1day(ci, freq = freq),
                   "rx5day" = climdex.pcic::climdex.rx5day(ci, freq = freq),
                   "sdii" = climdex.pcic::climdex.sdii(ci),
                   "r10mm" = climdex.pcic::climdex.r10mm(ci),
                   "r20mm" = climdex.pcic::climdex.r20mm(ci),
                   "rnnmm" = climdex.pcic::climdex.rnnmm(ci, threshold = threshold),
                   "cdd" = climdex.pcic::climdex.cdd(ci, spells.can.span.years = spells.can.span.years),
                   "cwd" = climdex.pcic::climdex.cwd(ci, spells.can.span.years = spells.can.span.years),
                   "r95ptot" = climdex.pcic::climdex.r95ptot(ci),
                   "r99ptot" = climdex.pcic::climdex.r99ptot(ci),
                   "prcptot" = climdex.pcic::climdex.prcptot(ci),
                   stop("index name ", indices[i], " not recognised.")
    )
    if (i == 1) {
      if (freq == "annual") {
        df_ind <- data.frame(names(vals), unname(vals))
        names(df_ind) <- c(year, indices[i])
      } else {
        df_ind <- data.frame(stringr::str_split_fixed(string = names(vals), n = 2, pattern = "-"), vals, row.names = NULL)
        names(df_ind) <- c(year, month, indices[i])
        df_ind[[month]] <- as.numeric(df_ind[[month]])
      }
    }
    else {
      df_ind[[indices[i]]] <- unname(vals)
    }
    if (indices[[i]] == "rnnmm") names(df_ind)[ncol(df_ind)] <- paste(indices[[i]], threshold, sep = "_")
  }
  return(df_ind)
}

