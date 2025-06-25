This folder has replication files for the paper "Local Linear Forests", by Friedberg, Tibshirani, Athey, and Wager.

All selected settings for the algorithms here are described in the paper.
Required R packages: grf, glmnet, BART, xgboost, rlearner, dplyr, ggplot2, splines, reshape2

Contained in this folder are:
* main.R
which runs all simulations found in the paper. Note that running time
(especially with tuning and the full replications) may be long. This file calls each of the files listed below.

* efficiency_controls.R
which lets users specify the degree of tuning and number of repetitions. 

*wages.R
Replicates Section 1.1: wage regression errors (Table 1), as main benchmark.
All variable and model names in the other files match this style.

*bottom_panel_table1.R
Replicates the lower panel of Table 1: mean squared errors in sparse regions (extreme ages, less-sampled races, large families).

*figure2.R
Replicates Figure 2: Observed vs. predicted log(wages) for large families.

*figure4.R
Replicates Figure 4: RMSE by model and dimension for the high-dimensional simulation.

*AppendixTable2.R
Replicates Table 2 (Appendix): simulation on the log-exp model.

*split_image.R
Replicates Figure 3: split frequencies for CART vs. local linear splitting.


Data:
update the path to your data (e.g., cps2018.csv).
