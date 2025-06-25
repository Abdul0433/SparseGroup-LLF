# This script loads cryptocurrency volatility forecasts and daily aggregated data,
# calculates market regimes, evaluates model performance (RMSE, QLIKE) by regime,
# performs non-parametric statistical tests (Kruskal-Wallis, Jonckheere-Terpstra,
# Conover-Iman) on squared errors, and saves all results to an Excel file.

library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(zoo)
library(Metrics)
library(ggplot2)
library(DescTools)
library(PMCMRplus)
library(writexl)

res_file <- "C:/Users/abdul/Documents/Extension_Thesis/Results/Forecast/rolling_results30_BTC.xlsx"
results <- read_xlsx(res_file, sheet = 1) %>%
  mutate(
    date = as.Date(date),
    actual = as.numeric(actual)
  )

agg_file <- "C:/Users/abdul/Documents/Extension_Thesis/Data/Daily_data/BTCUSDT_daily_aggregated.csv"
daily <- read_csv(agg_file) %>%
  mutate(date = as.Date(day)) %>%
  arrange(date) %>%
  mutate(
    ma_50 = rollmean(close, k = 50, fill = NA, align = "right"),
    ma_200 = rollmean(close, k = 200, fill = NA, align = "right"),
    perc_diff = (ma_50 - ma_200) / ma_200,
    regime = case_when(
      perc_diff > 0.01 ~ "Bull",
      perc_diff < -0.01 ~ "Bear",
      TRUE ~ "Sideways"
    )
  ) %>%
  dplyr::select(date, regime)

df <- results %>%
  left_join(daily, by = "date") %>%
  pivot_longer(
    cols = c(LLF, LassoLLF, SparseGroupLLF, RF, GARCH, HARRV, XGBoost),
    names_to = "model",
    values_to = "forecast"
  ) %>%
  filter(!is.na(regime), !is.na(forecast), forecast > 0, actual > 0)

metrics_by_regime <- df %>%
  group_by(regime, model) %>%
  summarise(
    RMSE = rmse(actual, forecast),
    QLIKE = mean((actual / forecast) - log(actual / forecast) - 1, na.rm = TRUE),
    .groups = "drop"
  )


df_se <- df %>%
  mutate(
    se = (actual - forecast)^2,
    regime = factor(regime, levels = c("Bull", "Bear", "Sideways")) 
  )

jt_se_results <- lapply(unique(df_se$model), function(m) {
  sub <- df_se %>% filter(model == m)
  jt <- JonckheereTerpstraTest(se ~ regime, data = sub, alternative = "increasing")
  data.frame(
    model = m,
    J_stat = as.numeric(jt$statistic),
    p_JT = jt$p.value,
    stringsAsFactors = FALSE
  )
})
jt_se_table <- bind_rows(jt_se_results)

kw_results <- lapply(unique(df_se$model), function(m) {
  sub <- df_se %>% filter(model == m)
  kw <- kruskal.test(se ~ regime, data = sub)
  data.frame(
    model = m,
    chi2 = kw$statistic,
    p_KW = kw$p.value,
    stringsAsFactors = FALSE
  )
})
kw_table <- bind_rows(kw_results)

ci_results <- lapply(unique(df_se$model), function(m) {
  sub <- df_se %>% filter(model == m)
  test <- kwAllPairsConoverTest(se ~ regime, data = sub, p.adjust.method = "holm")
  pmat <- as.data.frame(test$p.value)
  pmat <- tibble::rownames_to_column(pmat, var = "group1")
  dfp <- pivot_longer(pmat, -group1, names_to = "group2", values_to = "p.value")
  dfp$model <- m
  dfp
})
ci_table <- bind_rows(ci_results)

output_file <- "C:/Users/abdul/Documents/Extension_Thesis/Results/Forecast/regime_metrics_and_tests.xlsx"
write_xlsx(
  list(
    Metrics_by_Regime = metrics_by_regime,
    JT_SE_Results = jt_se_table,
    KW_SE_Results = kw_table,
    CI_SE_Results = ci_table
  ),
  path = output_file
)