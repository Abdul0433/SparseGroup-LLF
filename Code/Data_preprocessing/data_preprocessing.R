# This script processes raw hourly cryptocurrency data, aggregates it to daily,
# calculates various technical indicators and features, fetches external macro
# and sentiment data, merges all data, imputes missing values, and saves the
# clean, aggregated data for each cryptocurrency to a CSV file.
rm(list = ls())

set.seed(12345) # Setting a seed for reproducibility

library(dplyr)
library(lubridate)
library(readr)
library(TTR)
library(zoo)
library(quantmod)
library(httr)
library(jsonlite)

data_path <- "C:/Users/abdul/Documents/Extension_Thesis/Data/"
coins <- c("BTCUSDT", "ETHUSDT", "XRPUSDT", "DOGEUSDT", "SHIBUSDT", "XLMUSDT")

gtrends_df <- read.csv(paste0(data_path, "multiTimeline.csv"), skip = 2)
names(gtrends_df)[1:2] <- c("date", "google_trends")
gtrends_df$date <- as.Date(gtrends_df$date)
gtrends_df$google_trends <- as.numeric(gtrends_df$google_trends)



get_fear_greed <- function() {
  tryCatch({
    r <- GET("https://api.alternative.me/fng/?limit=0")
    data <- fromJSON(rawToChar(r$content))$data
    df <- data.frame(timestamp = as.POSIXct(as.numeric(data$timestamp), origin = "1970-01-01"),
                     fear_greed = as.numeric(data$value))
    df$date <- as.Date(df$timestamp)
    df[!duplicated(df$date), ]
  }, error = function(e) data.frame(date = as.Date(NA), fear_greed = NA))
}
fg_df <- get_fear_greed()

getSymbols(c("^VIX", "^GSPC", "^IXIC", "GC=F"), src = "yahoo", from = "2021-12-01")
macro_df <- merge(
  merge(
    merge(
      data.frame(date = index(VIX), vix = as.numeric(Cl(VIX))),
      data.frame(date = index(GSPC), sp500 = as.numeric(Cl(GSPC))),
      by = "date", all = TRUE
    ),
    data.frame(date = index(IXIC), nasdaq = as.numeric(Cl(IXIC))),
    by = "date", all = TRUE
  ),
  data.frame(date = index(`GC=F`), gold = as.numeric(Cl(`GC=F`))),
  by = "date", all = TRUE
)

for (coin in coins) {
  file <- paste0(data_path, coin, "_hourly_2022_2025.csv")
  df <- read_csv(file)
  
  df$open_time <- as.POSIXct(df$open_time, format = "%m/%d/%Y %H:%M", tz = "UTC")
  df <- df %>% arrange(open_time)
  df$date <- as.Date(df$open_time)
  df$hour <- hour(df$open_time)
  
  daily <- df %>%
    group_by(date) %>%
    summarise(
      open = first(open),
      high = max(high),
      low = min(low),
      close = last(close),
      volume = sum(volume),
      rv = first(rv),
      .groups = "drop"
    )
  
  atr_values <- ATR(cbind(daily$high, daily$low, daily$close), n = 14)
  
  daily <- daily %>%
    mutate(
      return = log(close / lag(close)),
      high_low_range = high - low,
      lag_rv = lag(rv),
      lag_return = lag(return),
      tr = atr_values[, "tr"],
      atr14 = atr_values[, "atr"],
      ma7 = SMA(close, n = 7),
      ma14 = SMA(close, n = 14),
      ma30 = SMA(close, n = 30),
      rsi14 = RSI(close, n = 14),
      roc9 = ROC(close, n = 9),
      mean_rv_5d = rollapply(rv, width = 5, FUN = mean, fill = NA, align = "right", na.rm = TRUE),
      sum_past_returns_3 = rollapply(return, width = 3, FUN = sum, fill = NA, align = "right", na.rm = TRUE),
      diff_past_returns_3 = return - lag(return, 3),
      weekend = ifelse(weekdays(date) %in% c("Saturday", "Sunday"), 1, 0),
      rv_week = rollapply(rv, width = 5, FUN = mean, fill = NA, align = "right", na.rm = TRUE),
      rv_month = rollapply(rv, width = 22, FUN = mean, fill = NA, align = "right", na.rm = TRUE)
    ) %>%
    left_join(fg_df, by = "date") %>%
    left_join(macro_df, by = "date") %>%
    left_join(btc_dominance_df, by = "date") %>%
    left_join(gtrends_df, by = "date")
  
  macro_vars <- c("vix", "sp500", "nasdaq", "gold", "google_trends")
  for (v in macro_vars) {
    if (v %in% names(daily)) {
      daily[[v]] <- zoo::na.locf(daily[[v]], na.rm = FALSE)
      daily[[v]] <- zoo::na.locf(daily[[v]], fromLast = TRUE)
    }
  }
  
  for (h in 0:23) {
    df[[paste0("hour_", h)]] <- as.integer(df$hour == h)
  }
  
  cols_to_remove <- setdiff(intersect(names(df), names(daily)), "date")
  df <- df %>% select(-all_of(cols_to_remove))
  
  df <- left_join(df, daily, by = "date")
  
  df <- na.omit(df) 
  
  if (nrow(df) > 0) {
    output_file <- paste0(data_path, coin, "_daily_aggregated.csv") 
    write.csv(df, output_file, row.names = FALSE)
  }
}