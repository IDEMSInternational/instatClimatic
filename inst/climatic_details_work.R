library(databook)
devtools::load_all()

data_book <- DataBook$new()

# Dialog: Import Dataset
new_RDS <- readRDS(file="C:/Users/lclem/OneDrive/Documents/data_for_testing_epicsa.RDS")
data_book$import_RDS(data_RDS=new_RDS)

rm(new_RDS)

# Dialog: Inventory
data_book$remove_current_filter(data_name="daily_niger")

daily_niger <- data_book$get_data_frame(data_name="daily_niger")
daily_niger <- daily_niger %>% dplyr::filter(station_name == "Agades")
date <- daily_niger$date
station_name <- daily_niger$station_name
instatClimatic::climatic_details(data=daily_niger, date=date, elements=c("sunh"), station=station_name,
                                 day  = TRUE,
                                 month = TRUE,
                                 year = TRUE,
                                 duplicates = "distinct")

instatClimatic::climatic_details(data=daily_niger, date=date, elements=c("sunh"), station=station_name,
                                 day  = TRUE,
                                 month = TRUE,
                                 year = TRUE,
                                 duplicates = "hierarchical")

instatClimatic::climatic_details(data=daily_niger, date=date, elements=c("sunh"), station=station_name,
                                 day  = TRUE,
                                 month = TRUE,
                                 year = TRUE,
                                 duplicates = "flag")





# station_name Element       From         To Count Level
# 1        Agades    sunh 1945-01-01 1952-10-25  2855   Day
# 2        Agades    sunh 1952-11-03 1952-11-03     1   Day
# 3        Agades    sunh 1957-03-08 1957-03-08     1   Day
# 4        Agades    sunh 1959-02-17 1959-02-17     1   Day
# 5        Agades    sunh 1959-08-01 1959-08-01     1   Day
# 6        Agades    sunh 1965-12-02 1965-12-04     3   Day
# 7        Agades    sunh 1966-02-17 1966-02-17     1   Day
# 8        Agades    sunh 1968-05-13 1968-05-13     1   Day
# 9        Agades    sunh 1970-05-21 1970-05-21     1   Day
# 10       Agades    sunh 1970-08-07 1970-08-07     1   Day
# 11       Agades    sunh 1970-10-18 1970-10-18     1   Day
# 12       Agades    sunh 1971-01-16 1971-01-16     1   Day
# 13       Agades    sunh 1974-01-04 1974-01-04     1   Day
# 14       Agades    sunh 1975-08-01 1975-08-31    31   Day
# 15       Agades    sunh 1977-06-27 1977-06-27     1   Day
# 16       Agades    sunh 1945-01-01 1952-09-30    93 Month
# 17       Agades    sunh 1975-08-01 1975-08-31     1 Month
# 18       Agades    sunh 1945-01-01 1951-12-31     7  Year