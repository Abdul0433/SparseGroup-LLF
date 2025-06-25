# This script processes cryptocurrency data to visualize realized volatility
# and closing prices, and to compute and display correlation matrices
# for key financial metrics across various coins.

library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(corrplot)
library(e1071)  


data_path <- "C:/Users/abdul/Documents/Extension_Thesis/Data/hourly_data/"
save_path <- "C:/Users/abdul/Documents/Extension_Thesis/Results/Summary statistics/"
coins <- c("BTCUSDT", "ETHUSDT", "XRPUSDT", "DOGEUSDT", "SHIBUSDT", "XLMUSDT")

summary_list <- list()

for (coin in coins) {
  file <- paste0(data_path, coin, "_with_predictors.csv")
  df <- read_csv(file, show_col_types = FALSE)
  
  df$date <- as.Date(df$date)
  
  p_rv <- ggplot(df, aes(x = date, y = rv)) +
    geom_line(color = "darkred") +
    labs(title = paste(coin, "Realized Volatility"),
         x = "Date",
         y = "RV") +
    theme_minimal()
  print(p_rv)
  
  ggsave(filename = paste0(save_path, coin, "_realized_volatility.png"),
         plot = p_rv, width = 8, height = 4)
  
  p_close <- ggplot(df, aes(x = date, y = close)) +
    geom_line(color = "steelblue") +
    labs(title = paste(coin, "Closing Price"),
         x = "Date",
         y = "Close Price") +
    theme_minimal()
  print(p_close)
  

ret <- na.omit(df$return*100)
ret <- ret[!is.na(ret)]

summary_list[[coin]] <- tibble(
  Coin       = coin,
  Mean       = mean(ret),
  Median     = median(ret),
  High       = max(ret),
  Low        = min(ret),
  St_Dev     = sd(ret),
  Skewness   = skewness(ret),
  Kurtosis   = kurtosis(ret),
  Observations = length(ret)
)
}

# Combineer alle rijen tot één tabel
summary_df <- bind_rows(summary_list)

# Opslaan als CSV
write_csv(summary_df, file = paste0(save_path, "summary_statistics_returns.csv"))

# Optioneel: print de tabel in R console
print(summary_df)
