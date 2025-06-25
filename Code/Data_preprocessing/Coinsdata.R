# This script downloads historical hourly cryptocurrency data from Binance
# for a specified list of coins and time period, then saves each coin's
# data to a separate CSV file.

library(binancer)
library(dplyr)
library(lubridate)

Sys.setlocale("LC_TIME", "C")

coins <- c("BTCUSDT", "ETHUSDT", "XRPUSDT", "DOGEUSDT", "SHIBUSDT", "XLMUSDT")

get_hourly_data <- function(symbol, start_date, end_date) {
  all_data <- data.frame()
  current <- ymd_hms(paste(start_date, "00:00:00"))
  end <- ymd_hms(paste(end_date, "00:00:00"))
  
  while (current < end) {
    next_time <- current + days(7)
    klines <- binance_klines(symbol, interval = "1h", start_time = current, end_time = next_time)
    all_data <- bind_rows(all_data, klines)
    current <- next_time
    Sys.sleep(1)
  }
  
  all_data$symbol <- symbol
  return(all_data)
}

for (coin in coins) {
  cat("Downloading:", coin, "\n")
  df <- get_hourly_data(coin, "2022-01-01", "2025-01-31")
  filename <- paste0(coin, "_hourly_2022_2025.csv")
  write.csv(df, filename, row.names = FALSE)
  cat("Saved:", filename, "\n\n")
}