###########################################
## Main file to call all other functions ##
###########################################

rm(list = ls())
set.seed(12345)

# set working directory 
setwd("C:/Users/abdul/Downloads/Thesis(replicate code)/code/code")




library(grf)
library(glmnet)
library(ggplot2)
library(BART)
library(xgboost)
library(rlearner)
library(bartCause)
library(dplyr)
library(splines)
library(reshape2)


# Efficient.run runs fewer repetitions and leaves out the largest dimensions and sample sizes.
efficient.run = TRUE

# To replicate results and get best XGB performance, set to TRUE
# Please note than when set to TRUE, code will take FAR longer to run, compared to tuning other methods. 
full.xgb.tune = FALSE

# Users can set num.reps and control more specifically in "efficiency_controls.R"
source("efficiency_controls.R")

#Empirical Wage Regression Example
source("wages.R")
source("bottom_panel_table1.R")  
source("figure2.R") 
#Section 2.1 Replication (Figures 3 and 4)
source("split_images.R")
source("figure4.R") 
#Replication of Appendix Table 2
source("AppendixTable2.R") 

