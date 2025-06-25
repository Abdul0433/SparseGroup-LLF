# This script calculates and saves market regimes (Bull, Bear, Sideways) for various
# cryptocurrencies based on moving average crossovers, and generates a plot for each.

library(dplyr)
library(zoo)
library(ggplot2)


coins     <- c("BTC", "ETH", "XRP", "DOGE", "SHIB", "XLM")
data_dir  <- "C:/Users/abdul/Documents/Extension_Thesis/Data/Daily_data/"
output_dir <- data_dir  

for (coin in coins) {
  file_in <- paste0(data_dir, coin, "USDT_daily_aggregated.csv")
  df <- read.csv(file_in) 
  df <- df %>%
    mutate(
      ma_50  = rollmean(close, k = 50,  fill = NA, align = "right"),
      ma_200 = rollmean(close, k = 200, fill = NA, align = "right"),
      perc_diff = (ma_50 - ma_200) / ma_200,
      regime    = case_when(
        perc_diff >  0.01   ~ "Bull",
        perc_diff < -0.01   ~ "Bear",
        TRUE                ~ "Sideways"
      )
    )
  
  
  write.csv(df, file_in, row.names=FALSE)
 
}
