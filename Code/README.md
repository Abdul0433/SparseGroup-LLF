# Cryptocurrency Volatility Forecasting Project

This repository contains R scripts used for a project on forecasting cryptocurrency volatility. The scripts cover data acquisition, preprocessing, exploratory data analysis, feature engineering, volatility forecasting using various models, and statistical testing.

## Scripts Overview

Here's a brief description of each R script:

Extension:
Coinsdata.R: Downloads historical hourly cryptocurrency data from Binance and saves it to CSV files. 
data_preprocessing.R: Processes raw hourly crypto data, aggregates to daily, calculates features, fetches external macro/sentiment data, imputes missing values, and saves cleaned data. 
EDA.R: Performs Exploratory Data Analysis by visualizing financial metrics and computing/displaying correlation matrices for cryptocurrency data. 
realized variance.R: Calculates daily realized volatility (RV) from hourly cryptocurrency data and merges it back into the hourly datasets. 
Forecast.R: Executes rolling window volatility forecasting using various models (LLF variants, RF, GARCH, HAR-RV, XGBoost), evaluates performance (RMSE, QLIKE), and saves results. 
Test.R: Conducts statistical tests (Friedman, Nemenyi) to assess the significance of performance differences among forecasting models based on forecast errors. 
MA50VS200.R: Calculates and saves market regimes (Bull, Bear, Sideways) for cryptocurrencies based on moving average crossovers, and visualizes them. 
Regimes.R: Evaluates forecasting model performance (RMSE, QLIKE) within defined market regimes and conducts non-parametric statistical tests (Kruskal-Wallis, Jonckheere-Terpstra, Conover-Iman) for regime-dependent analysis. 
Simulation.R: Conducts a Monte Carlo simulation to evaluate out-of-sample RMSE performance of machine learning models under controlled conditions (varying sample size, dimensionality, heteroscedasticity).


replication:
 main.R: which runs all simulations found in the paper. Note that running time
(especially with tuning and the full replications) may be long. This file calls each of the files listed below.
efficiency_controls.R: which lets users specify the degree of tuning and number of repetitions. 
Simulation2.R: code for simulation 2 in the appendix (simulation on the log-exp model).
Simulation3.R replicatates simulation 3 in the Appendix (split frequencies for CART vs. local linear splitting).
Simulation4.R: Replicates simulation 4 in appendix (RMSE by model and dimension for the high-dimensional simulation).
