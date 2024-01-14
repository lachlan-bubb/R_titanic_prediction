# Orchestration 

rm(list = ls()); gc();
setwd("~/kaggle/Titanic/")
source("01_logistic_regression.R")
source("02_RF.R")
source("03_xgb.R")
source("04_xgb_log_stacking.R")
source("05_xgb_rf_stacking.R")
source("06_xgb_CV.R")
