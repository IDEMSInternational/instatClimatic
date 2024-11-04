#' Cumulative Inventory
#'
#' Adds a cumulative count of missing values by period (and station if specified) to the data.
#'
#' @param data The input data frame.
#' @param station An optional column name indicating the station. Defaults to NULL.
#' @param from The start period column name.
#' @param to The end period column name.
#' @return A data frame with added cumulative counts.
#' @examples
#' \dontrun{
#'   cumulative_inventory(my_data, "station", "start_date", "end_date")
#' }
#' @export
cumulative_inventory <- function(data, station = NULL, from, to){
  if (is.null(station)){
    data <- data %>%
      dplyr::group_by(.data[[from]], .data[[to]]) %>%
      dplyr::mutate(cum=dplyr::n())
    data <- data %>%
      dplyr::group_by(.data[[from]]) %>%
      dplyr::mutate(cum1 = dplyr::n()) %>% 
      dplyr::mutate(cum1 = ifelse(cum == cum1, # are they all in the same period?
                                  yes = cum,  
                                  no = ifelse(cum == max(cum),
                                              cum,
                                              max(cum) + 0.5)))
  } else {
    data <- data %>%
      dplyr::group_by(.data[[station]], .data[[from]], .data[[to]]) %>%
      dplyr::mutate(cum=dplyr::n())
    data <- data %>%
      dplyr::group_by(.data[[station]], .data[[from]]) %>%
      dplyr::mutate(cum1 = dplyr::n()) %>% 
      dplyr::mutate(cum1 = ifelse(cum == cum1, # are they all in the same period?
                                  yes = cum,  
                                  no = ifelse(cum == max(cum),
                                              cum,
                                              max(cum) + 0.5)))
  }
  return(data)
}

#' Calculate Evaporation for Water Balance
#'
#' Calculates the evaporation based on water balance, fraction of capacity, and other parameters.
#'
#' @param water_balance The current water balance.
#' @param frac The fraction of capacity.
#' @param capacity The total capacity.
#' @param evaporation_value The value of evaporation.
#' @param rain The amount of rain.
#' @return The calculated evaporation.
#' @examples
#' \dontrun{
#'   WB_evaporation(100, 0.5, 200, 10, 5)
#' }
#' @export
WB_evaporation <- function(water_balance, frac, capacity, evaporation_value, rain){
  if (water_balance >= frac*capacity){
    evaporation <- evaporation_value
  } else {
    if (rain == 0){
      evaporation <- evaporation_value * ((water_balance)/(frac*capacity))
    } else {
      if (water_balance < frac*capacity){
        if (rain > evaporation_value){
          evaporation <- evaporation_value
        } else {
          evaporation <- evaporation_value * ((water_balance)/(frac*capacity))
          evaporation <- evaporation + ((evaporation_value - evaporation)*(rain/evaporation_value))
        }
      } else {
        evaporation <- evaporation_value
      }
    }
  }
  return(evaporation)
}

#' Write Weather Data to File
#'
#' Writes weather data to a text file with specified missing value codes.
#'
#' @param year A vector of years.
#' @param month A vector of months.
#' @param day A vector of days.
#' @param rain A vector of rainfall values.
#' @param mn_tmp A vector of minimum temperatures.
#' @param mx_tmp A vector of maximum temperatures.
#' @param missing_code The code to use for missing values.
#' @param output_file The name of the output file.
#' @return None. Writes the data to a file.
#' @examples
#' \dontrun{
#'   write_weather_data(2020, 1:12, 1:31, rain_data, min_temp, max_temp,
#'   -99, "weather.txt")
#' }
#' @export
write_weather_data <- function(year, month, day, rain, mn_tmp, mx_tmp, missing_code, output_file) {
  # Create a data frame with the provided inputs
  weather_data <- data.frame(year = year,
                             month = month,
                             day = day,
                             rain = rain,
                             mn_tmp = mn_tmp,
                             mx_tmp = mx_tmp)
  
  # Replace missing values with the specified code
  weather_data[is.na(weather_data)] <- missing_code
  
  # Write the data frame to a text file
  utils::write.table(weather_data, file = output_file, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
  
  cat("Weather data has been written to", output_file, "\n")
}

#' Prepare Data for Walter-Lieth Plot
#'
#' Prepares data for Walter-Lieth climate diagrams.
#'
#' @param data The input data frame.
#' @param month The column name for months.
#' @param tm_min The column name for minimum temperature.
#' @param ta_min The column name for average temperature.
#' @return A list of data frames and plots prepared for Walter-Lieth climate diagrams.
#' @examples
#' \dontrun{
#'   prepare_walter_lieth(my_data, "Month", "MinTemp", "AvgTemp")
#' }
#' @export
prepare_walter_lieth <- function(data, month, tm_min, ta_min){
  dat_long_int <- NULL
  for (j in seq(nrow(data) - 1)) {
    intres <- NULL
    for (i in seq_len(ncol(data))) {
      if (is.character(data[j, i]) | is.factor(data[j, i])) {
        val <- as.data.frame(data[j, i])
      }
      else {
        interpol <- stats::approx(x = data[c(j, j + 1), "indrow"],
                           y = data[c(j, j + 1), i],
                           n = 50)
        val <- as.data.frame(interpol$y)
      }
      names(val) <- names(data)[i]
      intres <- dplyr::bind_cols(intres, val)
    }
    dat_long_int <- dplyr::bind_rows(dat_long_int, intres)
  }
  dat_long_int$interpolate <- TRUE
  dat_long_int[[month]] <- ""
  data$interpolate <- FALSE
  dat_long_int <- dat_long_int[!dat_long_int$indrow %in% data$indrow, ]
  dat_long_end <- dplyr::bind_rows(data, dat_long_int)
  dat_long_end <- dat_long_end[order(dat_long_end$indrow), ]
  dat_long_end <- dat_long_end[dat_long_end$indrow >= 0 & dat_long_end$indrow <= 12, ]
  dat_long_end <- tibble::as_tibble(dat_long_end)
  
  getpolymax <- function(x, y, y_lim) {
    initpoly <- FALSE
    yres <- NULL
    xres <- NULL
    for (i in seq_len(length(y))) {
      lastobs <- i == length(x)
      if (y[i] > y_lim[i]) {
        if (isFALSE(initpoly)) {
          xres <- c(xres, x[i])
          yres <- c(yres, y_lim[i])
          initpoly <- TRUE
        }
        xres <- c(xres, x[i])
        yres <- c(yres, y[i])
        if (lastobs) {
          xres <- c(xres, x[i], NA)
          yres <- c(yres, y_lim[i], NA)
        }
      }
      else {
        if (initpoly) {
          xres <- c(xres, x[i - 1], NA)
          yres <- c(yres, y_lim[i - 1], NA)
          initpoly <- FALSE
        }
      }
    }
    poly <- tibble::tibble(x = xres, y = yres)
    return(poly)
  }
  getlines <- function(x, y, y_lim) {
    yres <- NULL
    xres <- NULL
    ylim_res <- NULL
    for (i in seq_len(length(y))) {
      if (y[i] > y_lim[i]) {
        xres <- c(xres, x[i])
        yres <- c(yres, y[i])
        ylim_res <- c(ylim_res, y_lim[i])
      }
    }
    line <- tibble::tibble(x = xres, y = yres, ylim_res = ylim_res)
    return(line)
  }
  prep_max_poly <- getpolymax(x = dat_long_end$indrow, y = pmax(dat_long_end$pm_reesc, 
                                                                50), y_lim = rep(50, length(dat_long_end$indrow)))
  tm_max_line <- getlines(x = dat_long_end$indrow, y = dat_long_end$tm, 
                          y_lim = dat_long_end$pm_reesc)
  pm_max_line <- getlines(x = dat_long_end$indrow, y = pmin(dat_long_end$pm_reesc, 
                                                            50), y_lim = dat_long_end$tm)
  dat_real <- dat_long_end[dat_long_end$interpolate == FALSE, 
                           c("indrow", ta_min)]
  x <- NULL
  y <- NULL
  for (i in seq_len(nrow(dat_real))) {
    if (dat_real[i, ][[ta_min]] < 0) {
      x <- c(x, NA, rep(dat_real[i, ]$indrow - 0.5, 2), 
             rep(dat_real[i, ]$indrow + 0.5, 2), NA)
      y <- c(y, NA, -3, 0, 0, -3, NA)
    }
    else {
      x <- c(x, NA)
      y <- c(y, NA)
    }
  }
  probfreeze <- tibble::tibble(x = x, y = y)
  rm(dat_real)
  dat_real <- dat_long_end[dat_long_end$interpolate == FALSE, 
                           c("indrow", tm_min)]
  x <- NULL
  y <- NULL
  for (i in seq_len(nrow(dat_real))) {
    if (dat_real[i, ][[tm_min]] < 0) {
      x <- c(x, NA, rep(dat_real[i, ]$indrow - 0.5, 2), 
             rep(dat_real[i, ]$indrow + 0.5, 2), NA)
      y <- c(y, NA, -3, 0, 0, -3, NA)
    }
    else {
      x <- c(x, NA)
      y <- c(y, NA)
    }
  }
  surefreeze <- tibble::tibble(x = x, y = y)
  return_list <- list(dat_long_end,
                      tm_max_line,
                      pm_max_line,
                      prep_max_poly,
                      probfreeze,
                      surefreeze)
  names(return_list) <- c("dat_long_end", "tm_max_line", "pm_max_line",
                          "prep_max_poly", "prob_freeze", "surefreeze")
  return(return_list)
}

#' Generate Walter-Lieth Plot
#'
#' Generates Walter-Lieth climate diagrams using ggplot2.
#'
#' @param data The input data frame.
#' @param month The column name for months.
#' @param station An optional column name for station. Defaults to NULL.
#' @param p_mes The column name for precipitation measurements.
#' @param tm_max The column name for maximum temperature.
#' @param tm_min The column name for minimum temperature.
#' @param ta_min The column name for average temperature.
#' @param station_name The name of the station. Defaults to an empty string.
#' @param alt The altitude. Defaults to NA.
#' @param per The period. Defaults to NA.
#' @param pcol The color for precipitation lines. Defaults to "#002F70".
#' @param tcol The color for temperature lines. Defaults to "#ff0000".
#' @param pfcol The fill color for probable freeze areas. Defaults to "#9BAEE2".
#' @param sfcol The fill color for sure freeze areas. Defaults to "#3C6FC4".
#' @param shem Logical indicating whether the station is in the southern hemisphere. Defaults to FALSE.
#' @param p3line Logical indicating whether to plot the precipitation/3 line. Defaults to FALSE.
#' @param ... Additional arguments to be passed to ggplot2 functions.
#' @return A ggplot2 object with the Walter-Lieth climate diagram.
#' @examples
#' \dontrun{
#'   ggwalter_lieth(my_data, "Month", NULL, "Precipitation",
#'   "MaxTemp", "MinTemp", "AvgTemp")
#' }
#' @export
ggwalter_lieth <- function (data, month, station = NULL, p_mes, tm_max, tm_min, ta_min, station_name = "", 
                            alt = NA, per = NA, pcol = "#002F70", 
                            tcol = "#ff0000", pfcol = "#9BAEE2", sfcol = "#3C6FC4", 
                            shem = FALSE, p3line = FALSE, ...) 
{
  
  # Preprocess data with vectorised operations
  data <- data %>%
    dplyr::mutate(tm = (.data[[tm_max]] + .data[[tm_min]]) / 2,
                  pm_reesc = dplyr::if_else(.data[[p_mes]] < 100, .data[[p_mes]] * 0.5, .data[[p_mes]] * 0.05 + 45),
                  p3line = .data[[p_mes]] / 3) %>%
    dplyr::mutate(dplyr::across(.data[[month]], ~ forcats::fct_expand(.data[[month]], ""))) %>%
    dplyr::arrange(.data[[month]])
  # do this for each station, if we have a station
  if (!is.null(station)){
    data <- data %>% dplyr::group_by(!!rlang::sym(station))
  }
  data <- data %>%
    dplyr::group_modify(~{
      # Add dummy rows at the beginning and end for each group
      .x <- bind_rows(.x[nrow(.x), , drop = FALSE], .x, .x[1, , drop = FALSE])
      # Clear month value for the dummy rows
      .x[c(1, nrow(.x)), which(names(.x) == data[[month]])] <- ""
      # Add an index column for plotting or further transformations
      .x <- cbind(indrow = seq(-0.5, 12.5, 1), .x)
      .x
    })
  
  if (!is.null(station)){
    data <- data %>% dplyr::ungroup()
  }
  data <- data.frame(data)
  
  # split by station
  if (is.null(station)){
    data_list <- prepare_walter_lieth(data, month, tm_min, ta_min)
    # data things
    dat_long_end <- data_list$dat_long_end
    tm_max_line <- data_list$tm_max_line
    pm_max_line <- data_list$pm_max_line
    prep_max_poly <- data_list$prep_max_poly
    probfreeze <- data_list$prob_freeze
    surefreeze <- data_list$surefreeze
  } else {
    results <-
      purrr::map(.x = unique(data[[station]]),
          .f = ~{filtered_data <- data %>% dplyr::filter(!!rlang::sym(station) == .x)
          prepare_walter_lieth(filtered_data, month, tm_min, ta_min)})
    # Function to bind rows for a specific sub-element across all main elements
    n <- length(results)
    m <- length(results[[1]])
    station_name <- unique(data[[station]])
    binds <- NULL
    combined <- NULL
    for (j in 1:m){
      for (i in 1:n) { # for each station data set
        binds[[i]] <- results[[i]][[j]] %>% dplyr::mutate(!!rlang::sym(station) := station_name[i])
      }
      combined[[j]] <- do.call(rbind, binds) # Combine all the sub-elements row-wise
    }
    # data things
    dat_long_end <- combined[[1]]
    tm_max_line <- combined[[2]]
    pm_max_line <- combined[[3]]
    prep_max_poly <- combined[[4]]
    probfreeze <- combined[[5]]
    surefreeze <- combined[[6]]
  }
  
  # data frame pretty things ------------------------------------------------------
  ticks <- data.frame(x = seq(0, 12), ymin = -3, ymax = 0)
  month_breaks <- dat_long_end[dat_long_end[[month]] != "", ]$indrow
  month_labs <- dat_long_end[dat_long_end[[month]] != "", ][[month]]
  
  ymax <- max(60, 10 * floor(max(dat_long_end$pm_reesc)/10) + 10)
  ymin <- min(-3, min(dat_long_end$tm))
  range_tm <- seq(0, ymax, 10)
  if (ymin < -3) {
    ymin <- floor(ymin/10) * 10
    range_tm <- seq(ymin, ymax, 10)
  }
  templabs <- paste0(range_tm)
  templabs[range_tm > 50] <- ""
  range_prec <- range_tm * 2
  range_prec[range_tm > 50] <- range_tm[range_tm > 50] * 20 - 900
  preclabs <- paste0(range_prec)
  preclabs[range_tm < 0] <- ""
  
  wandlplot <- ggplot2::ggplot() + ggplot2::geom_line(data = dat_long_end, 
                                                      ggplot2::aes(x = .data$indrow, y = .data$pm_reesc), color = pcol) + 
    ggplot2::geom_line(data = dat_long_end, ggplot2::aes(x = .data$indrow, y = .data$tm), color = tcol)
    
  if (nrow(tm_max_line > 0)) {
    wandlplot <- wandlplot + ggplot2::geom_segment(ggplot2::aes(x = .data$x, 
                                                       y = .data$ylim_res, xend = .data$x, yend = .data$y), 
                                                   data = tm_max_line, color = tcol, alpha = 0.2)
  }
  if (nrow(pm_max_line > 0)) {
    wandlplot <- wandlplot + ggplot2::geom_segment(ggplot2::aes(x = .data$x, 
                                                       y = .data$ylim_res, xend = .data$x, yend = .data$y), 
                                                   data = pm_max_line, color = pcol, alpha = 0.2)
  }
  if (p3line) {
    wandlplot <- wandlplot + ggplot2::geom_line(data = dat_long_end, 
                                                ggplot2::aes(x = .data$indrow, y = .data$p3line), color = pcol)
  }
  if (max(dat_long_end$pm_reesc) > 50) {
    wandlplot <- wandlplot + ggplot2::geom_polygon(data = prep_max_poly, ggplot2::aes(x, y),
                                                   fill = pcol)
  }
  if (min(dat_long_end[[ta_min]]) < 0) {
    wandlplot <- wandlplot + ggplot2::geom_polygon(data = probfreeze, ggplot2::aes(x = x, y = y),
                                                   fill = pfcol, colour = "black")
  }
  if (min(dat_long_end[[tm_min]]) < 0) {
    wandlplot <- wandlplot + ggplot2::geom_polygon(data = surefreeze, ggplot2::aes(x = x, y = y),
                                          fill = sfcol, colour = "black")
  }
  wandlplot <- wandlplot + ggplot2::geom_hline(yintercept = c(0, 50), 
                                      linewidth = 0.5) +
    ggplot2::geom_segment(data = ticks, ggplot2::aes(x = x, xend = x, y = ymin, yend = ymax)) +
    ggplot2::scale_x_continuous(breaks = month_breaks, name = "", labels = month_labs, expand = c(0, 0)) + 
    ggplot2::scale_y_continuous("C", limits = c(ymin, ymax), labels = templabs, 
                       breaks = range_tm, sec.axis = ggplot2::dup_axis(name = "mm", labels = preclabs))
  wandlplot <- wandlplot +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.line.x.bottom = ggplot2::element_blank(), 
                   axis.title.y.left = ggplot2::element_text(angle = 0, 
                                                    vjust = 0.9, size = 10, colour = tcol,
                                                    margin = ggplot2::unit(rep(10, 4), "pt")),
                   axis.text.x.bottom = ggplot2::element_text(size = 10), 
                   axis.text.y.left = ggplot2::element_text(colour = tcol, size = 10), 
                   axis.title.y.right = ggplot2::element_text(angle = 0, vjust = 0.9, 
                                                     size = 10, colour = pcol,
                                                     margin = ggplot2::unit(rep(10, 4), "pt")),
                   axis.text.y.right = ggplot2::element_text(colour = pcol, size = 10))
  
  if (!is.null(station)){
    wandlplot <- wandlplot + ggplot2::facet_wrap(station)
  }
  
  return(wandlplot)
}