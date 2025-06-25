# This script loads hourly cryptocurrency data, calculates daily realized volatility,
# merges it back into the hourly data, and then overwrites the original hourly CSV files.

library(dplyr)
library(lubridate)
library(readr)

data_path <- "C:/Users/abdul/Documents/Extension_Thesis/Data/"

BTCUSDT <- read_csv(paste0(data_path, "BTCUSDT_hourly_2022_2025.csv"))
ETHUSDT <- read_csv(paste0(data_path, "ETHUSDT_hourly_2022_2025.csv"))
XRPUSDT <- read_csv(paste0(data_path, "XRPUSDT_hourly_2022_2025.csv"))
DOGEUSDT <- read_csv(paste0(data_path, "DOGEUSDT_hourly_2022_2025.csv"))
SHIBUSDT <- read_csv(paste0(data_path, "SHIBUSDT_hourly_2022_2025.csv"))
XLMUSDT <- read_csv(paste0(data_path, "XLMUSDT_hourly_2022_2025.csv"))

coin_data <- list(
  BTCUSDT = BTCUSDT,
  ETHUSDT = ETHUSDT,
  XRPUSDT = XRPUSDT,
  DOGEUSDT = DOGEUSDT,
  SHIBUSDT = SHIBUSDT,
  XLMUSDT = XLMUSDT
)

for (coin in names(coin_data)) {
  df <- coin_data[[coin]]
  
  df$open_time <- as.POSIXct(df$open_time, format = "%m/%d/%Y %H:%M", tz = "UTC")
  
  df <- df %>%
    arrange(open_time) %>%
    mutate(log_return = log(close / lag(close)),
           date = as.Date(open_time))
  
  rv_df <- df %>%
    group_by(date) %>%
    summarise(rv = sqrt(sum(log_return^2, na.rm = TRUE)), .groups = "drop")
  
  df <- left_join(df, rv_df, by = "date")
  
  output_file <- paste0(data_path, coin, "_hourly_2022_2025.csv")
  write.csv(df, output_file, row.names = FALSE)
}