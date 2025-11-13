#' Climatic Details
#' 
#' @description
#' This function extracts climatic details from a given dataset based on specified parameters such as date, elements, stations, and level. It provides information about the duration and frequency of missing values for each element at different levels (day, month, year).
#' 
#' @param data A dataset containing climatic data.
#' @param date The date variable in the dataset.
#' @param elements A character vector specifying the climatic elements to analyse.
#' @param stations A character vector specifying the stations to analyse.
#' @param order A logical value indicating whether the resulting tables should be ordered.
#' @param day A logical value indicating whether to include information at the day level.
#' @param month A logical value indicating whether to include information at the month level.
#' @param year A logical value indicating whether to include information at the year level.
#' @param level A logical value indicating whether to include the level information in the output.
#' @param duplicates How to handle duplicated or overlapping missing spells
#'   across different temporal levels (day, month, year). A "duplicate"
#'   in the strict sense is a spell with the same station, element, start
#'   date (\code{From}) and end date (\code{To}) that appears at more
#'   than one level. Options are:
#'   \itemize{
#'     \item \code{"distinct"} (default): Keep only the highest-level spell in
#'       each exact-duplicate group (Year > Month > Day). The number of
#'       omitted rows is reported as a message.
#'     \item \code{"flag"}: Keep all rows and add a \code{duplicate_index}
#'       column. The first occurrence in each exact-duplicate group has
#'       \code{duplicate_index = 1}, later duplicates have values
#'       \code{> 1}. A message reports how many rows were flagged as
#'       duplicates.
#'     \item \code{"keep"}: Keep all rows and do not add any duplicate
#'       index or message.
#'     \item \code{"hierarchical"}: Decompose overlapping spells so that each
#'       date is represented only once, at the highest available level
#'       (Year > Month > Day). For example, a long daily spell that
#'       contains whole missing months and years is split into:
#'       full years (Year), remaining full months (Month), and remaining
#'       individual days (Day). A message reports how many original rows
#'       were omitted due to this compression.
#'   }
#'   
#' @return A data frame containing climatic details such as start and end dates of missing values and the count of missing values for each element at the specified levels.
#'
#' @details
#' When multiple temporal levels are requested (e.g., \code{day = TRUE},
#' \code{month = TRUE}, \code{year = TRUE}), the same missing spell can
#' appear at several levels. For example, if an entire year is missing,
#' the corresponding spell may appear as one daily spell, one monthly spell
#' and one yearly spell with identical \code{From} and \code{To} dates.
#'
#' The \code{duplicates} argument controls how such overlaps are handled.
#' By default (\code{duplicates = "distinct"}), only the highest-level spell
#' (Year > Month > Day) is kept for exact duplicates and the number of
#' omitted rows is reported in a message. Using \code{duplicates = "flag"}
#' retains all rows and adds a \code{duplicate_index} column so that users
#' can filter or summarise duplicates as needed. Setting
#' \code{duplicates = "keep"} disables this behaviour and returns all
#' spells unchanged.
#'
#' The option \code{duplicates = "hierarchical"} performs a finer-grained
#' compression: overlapping spells at different levels are decomposed so
#' that each date is only counted once, at the highest level available
#' (Year > Month > Day). For example, if a long daily spell contains
#' several full years and months of missing data, the result is split into
#' separate Year, Month and Day spells covering those periods.
#' 
#' @export
#'
#' @examples
#' data(ghana)
#' 
#' climatic_details(data = ghana,
#'                  date = date,
#'                  stations = station,
#'                  elements = c("rainfall", "min_temperature"),
#'                  month = TRUE
#'                  )
climatic_details <- function(data, date, elements, stations,
                             order = TRUE,
                             day = FALSE,
                             month = TRUE,
                             year = FALSE,
                             level = FALSE,
                             duplicates = c("distinct", "flag", "keep", "hierarchical")){
                             
  if (missing(date)){
    stop('argument "date" is missing, with no default')
  }
  
  if (missing(elements)){
    stop('argument "elements" is missing, with no default')
  }
  
  duplicates  <- match.arg(duplicates)
  station_sym <- rlang::ensym(stations)
  station_col <- rlang::as_string(station_sym)
  
  i <- 0
  list_tables <- NULL
  
  # stack data
  data.stack <- data %>%
    tidyr::pivot_longer(cols = c({{ elements }}),
                        names_to = "Element",
                        values_to = "Value") %>%
    dplyr::mutate(Element = instatExtras::make_factor(Element))
  
  if (!any(day, month, year)){
    warning('At least one of day, month, year need to be selected')
  }
  
  if (day){
    i <- i + 1
    detail.table.day <- data.stack %>%
      dplyr::group_by({{ stations }}, Element) %>%
      dplyr::mutate(element.na = data.table::rleid(Value)) %>%
      dplyr::filter(is.na(Value)) %>%
      dplyr::group_by(element.na, {{ stations }}, Element) %>%
      dplyr::summarise(From = dplyr::first({{ date }}),
                       To   = dplyr::last({{ date }}),
                       Count = dplyr::n(),
                       .groups = "drop_last") %>%
      dplyr::mutate(Level = "Day")
    
    if (order){
      detail.table.day <- detail.table.day %>% dplyr::arrange(From)
    } else {  
      detail.table.day <- detail.table.day %>% dplyr::arrange(Element)
    }
    
    detail.table.day <- detail.table.day %>%
      dplyr::ungroup() %>%
      dplyr::select(-"element.na")
    
    list_tables[[i]] <- detail.table.day
  }
  
  if (month){
    i <- i + 1
    detail.table.month <- data.stack %>%
      dplyr::mutate(Date.ym = zoo::as.yearmon({{ date }}))  %>%
      dplyr::group_by(Date.ym, {{ stations }}, Element) %>%
      dplyr::summarise(no = dplyr::n(),
                       na = sum(is.na(Value)),
                       From = dplyr::first({{ date }}),
                       To   = dplyr::last({{ date }}),
                       .groups = "drop_last") %>%
      dplyr::mutate(is.complete = ifelse(no == na, 1, 0)) %>%
      dplyr::group_by({{ stations }}, Element) %>%
      dplyr::mutate(element.na = data.table::rleid(is.complete)) %>%
      dplyr::filter(is.complete == 1) %>%
      dplyr::group_by(element.na, {{ stations }}, Element) %>%
      dplyr::summarise(From = dplyr::first(From),
                       To   = dplyr::last(To),
                       Count = dplyr::n(),
                       .groups = "drop_last") %>%
      dplyr::mutate(Level = "Month")
    
    if (order){
      detail.table.month <- detail.table.month %>% dplyr::arrange(From)
    } else {
      detail.table.month <- detail.table.month %>% dplyr::arrange(Element)
    }
    
    detail.table.month <- detail.table.month %>%
      dplyr::ungroup() %>%
      dplyr::select(-"element.na")
    
    list_tables[[i]] <- detail.table.month
  }
  
  if (year) {
    i <- i + 1
    detail.table.year <- data.stack %>%
      dplyr::mutate(Date.y = lubridate::year({{ date }}))  %>%
      dplyr::group_by(Date.y, {{ stations }}, Element) %>%
      dplyr::summarise(no = dplyr::n(),
                       na = sum(is.na(Value)),
                       From = dplyr::first({{ date }}),
                       To   = dplyr::last({{ date }}),
                       .groups = "drop_last") %>%
      dplyr::mutate(is.complete = ifelse(no == na, 1, 0)) %>%
      dplyr::group_by({{ stations }}, Element) %>%
      dplyr::mutate(element.na = data.table::rleid(is.complete)) %>%
      dplyr::filter(is.complete == 1) %>%
      dplyr::group_by(element.na, {{ stations }}, Element) %>%
      dplyr::summarise(From = dplyr::first(From),
                       To   = dplyr::last(To),
                       Count = dplyr::n(),
                       .groups = "drop_last") %>%
      dplyr::mutate(Level = "Year")
    
    if (order){
      detail.table.year <- detail.table.year %>% dplyr::arrange(From)
    } else {
      detail.table.year <- detail.table.year %>% dplyr::arrange(Element)
    }
    
    detail.table.year <- detail.table.year %>%
      dplyr::ungroup() %>%
      dplyr::select(-"element.na")
    
    list_tables[[i]] <- detail.table.year
  }
  
  detail.table.all <- plyr::ldply(list_tables, data.frame)
  
  # Ensure Level is a factor
  if ("Level" %in% names(detail.table.all)){
    detail.table.all <- detail.table.all %>%
      dplyr::mutate(Level = instatExtras::make_factor(Level))
  }
  
  ## ---- duplicates = "distinct" / "flag": exact duplicate handling ----
  if (duplicates %in% c("distinct", "flag") && nrow(detail.table.all) > 0) {
    
    detail.table.all <- detail.table.all %>%
      dplyr::group_by(!!station_sym, Element, From, To) %>%
      # Prioritise Year > Month > Day for keeping
      dplyr::arrange(dplyr::case_when(
        Level == "Year"  ~ 1L,
        Level == "Month" ~ 2L,
        Level == "Day"   ~ 3L,
        TRUE             ~ 4L
      ), .by_group = TRUE) %>%
      dplyr::mutate(duplicate_index = dplyr::row_number()) %>%
      dplyr::ungroup()
    
    n_duplicates <- sum(detail.table.all$duplicate_index > 1, na.rm = TRUE)
    
    if (duplicates == "distinct") {
      
      if (n_duplicates > 0) {
        message(n_duplicates, " rows in details omitted as duplicates")
      }
      
      detail.table.all <- detail.table.all %>%
        dplyr::filter(duplicate_index == 1) %>%
        dplyr::select(-duplicate_index)
      
    } else if (duplicates == "flag") {
      
      if (n_duplicates > 0) {
        message(n_duplicates, " rows in details flagged as duplicates (duplicate_index > 1)")
      }
      # duplicate_index kept in output
    }
  }
  
  ## ---- duplicates = "hierarchical": hierarchical compression ----
  if (duplicates == "hierarchical" && nrow(detail.table.all) > 0) {
    n_before <- nrow(detail.table.all)
    
    detail.table.all <- detail.table.all %>%
      dplyr::group_by(!!station_sym, Element) %>%
      dplyr::group_modify(~ .compress_spells_drop(.x, station_col = station_col)) %>%
      dplyr::ungroup()
    
    n_after   <- nrow(detail.table.all)
    n_omitted <- n_before - n_after
    
    if (n_omitted > 0) {
      message(n_omitted, " rows in details omitted as overlaps (hierarchy Year > Month > Day)")
    }
  }
  
  return(detail.table.all)
}

#' Compress Spells Drop
#' 
#' @description
#' A helper function for `climatic_details` under the `hierarchical` option.
#'  
#' @param df A dataset containing climatic data.
#' @param station_col A character vector specifying the stations to analyse.
#' 
#' @return compressed data frame
.compress_spells_drop <- function(df, station_col) {
  # df: single station + Element
  # must contain: From, To, Level, Element, station_col
  
  if (nrow(df) == 0) return(df[0, , drop = FALSE])
  
  # Ensure From/To are Date
  df$From <- as.Date(df$From)
  df$To   <- as.Date(df$To)
  
  # Full date range
  all_dates  <- seq(min(df$From), max(df$To), by = "day")
  level_code <- integer(length(all_dates))  # 0 = not missing at any level
  
  apply_level <- function(sub, code) {
    if (nrow(sub) == 0) return()
    sub$From <- as.Date(sub$From)
    sub$To   <- as.Date(sub$To)
    for (i in seq_len(nrow(sub))) {
      idx <- all_dates >= sub$From[i] & all_dates <= sub$To[i]
      level_code[idx] <<- pmax(level_code[idx], code)
    }
  }
  
  # Apply: Day < Month < Year in priority
  apply_level(df[df$Level == "Day",   , drop = FALSE], 1L)
  apply_level(df[df$Level == "Month", , drop = FALSE], 2L)
  apply_level(df[df$Level == "Year",  , drop = FALSE], 3L)
  
  # Runs must be computed BEFORE filtering out zeros
  runs_all <- data.table::rleid(level_code)
  
  keep <- level_code > 0
  if (!any(keep)) return(df[0, , drop = FALSE])
  
  dates_kept <- all_dates[keep]
  codes_kept <- level_code[keep]
  runs_kept  <- runs_all[keep]
  
  # Group consecutive dates with the same code AND same run id
  res <- dplyr::tibble(
    From      = tapply(dates_kept, runs_kept, min),
    To        = tapply(dates_kept, runs_kept, max),
    LevelCode = tapply(codes_kept, runs_kept, unique)
  )
  
  res$From <- as.Date(res$From)
  res$To   <- as.Date(res$To)
  
  # Map LevelCode back to Level
  lev_levels <- levels(df$Level)
  code_to_level <- function(x) dplyr::case_when(
    x == 3L ~ "Year",
    x == 2L ~ "Month",
    x == 1L ~ "Day",
    TRUE    ~ NA_character_
  )
  res$Level <- factor(code_to_level(res$LevelCode), levels = lev_levels)
  
  # Count in correct units
  res$Count <- dplyr::case_when(
    res$Level == "Day" ~ as.integer(res$To - res$From + 1L),
    res$Level == "Month" ~ {
      12L * (lubridate::year(res$To) - lubridate::year(res$From)) +
        (lubridate::month(res$To) - lubridate::month(res$From)) + 1L
    },
    res$Level == "Year" ~ {
      as.integer(lubridate::year(res$To) - lubridate::year(res$From) + 1L)
    },
    TRUE ~ NA_integer_
  )
  
  # Add station + Element back
  res[[station_col]] <- df[[station_col]][1]
  res$Element        <- df$Element[1]
  
  # Reorder to match original df (drop LevelCode)
  res <- res[, names(df), drop = FALSE]
  res
}