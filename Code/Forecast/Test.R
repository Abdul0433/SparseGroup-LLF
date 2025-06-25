# R script for Friedman test and Nemenyi post-hoc test based on raw forecast errors

library(readxl)
library(tidyr)
library(dplyr)
library(PMCMRplus)

# Define coins and base path
tickers <- c("BTC", "DOGE", "ETH", "SHIB", "XLM", "XRP")
base_path <- "C:/Users/abdul/Documents/Extension_Thesis/Results/Forecast/"

generate_qlike <- function(actual, pred) {
  if (actual > 0 && pred > 0) {
    a <- actual / pred
    return(a - log(a) - 1)
  } else {
    return(NA_real_)
  }
}

for (coin in tickers) {
  file <- file.path(base_path, paste0("rolling_results30_", coin, ".xlsx"))
  
  res <- read_excel(
    path = file,
    sheet = "Results",
    col_types = c(
      "text",
      rep("numeric", ncol(read_excel(path = file, sheet = "Results")[, -1]))
    )
  ) %>%
    as_tibble()
  
  err_long <- res %>%
    pivot_longer(
      cols = -c(date, actual),
      names_to = "model",
      values_to = "pred"
    ) %>%
    mutate(
      model = as.character(model),
      date = as.character(date)
    ) %>%
    rowwise() %>%
    mutate(
      AbsErr = abs(pred - actual),
      QLIKE_err = generate_qlike(actual, pred)
    ) %>%
    ungroup() %>%
    filter(!is.na(QLIKE_err))
  
  for (met in c("AbsErr", "QLIKE_err")) {
    cat(sprintf("\n=== Coin: %s | Metric: %s ===\n", coin, met))
    
    df_wide <- err_long %>%
      select(date, model, !!sym(met)) %>%
      pivot_wider(
        names_from = model,
        values_from = !!sym(met),
        values_fn = mean,
        values_fill = NA_real_
      ) %>%
      drop_na()
    
    data_mat <- as.matrix(select(df_wide, -date))
    colnames(data_mat) <- colnames(df_wide)[-1]
    rownames(data_mat) <- df_wide$date
    
    if (nrow(data_mat) < 2 || ncol(data_mat) < 2) {
      cat("Not enough data for test\n")
      next
    }
  
    ft <- friedman.test(data_mat)
    cat(sprintf("Friedman chi-squared = %.4f, p = %.4g\n", ft$statistic, ft$p.value))
    
  
    ph_obj <- frdAllPairsNemenyiTest(data_mat)
    print(ph_obj$p.value)
  }
}
