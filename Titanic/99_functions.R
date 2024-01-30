###############################
## Titanic Functions
## Lachlan Bubb
## 04/17/2020
###############################
rm(list = ls());

# setwd("~/Titanic/")

package_list = c(
  "data.table",
  "ggplot2",
  "foreach",
  "pdftools",
  "magrittr",
  "gridExtra",
  "corrplot",
  "randomForest",
  "xgboost"
)

# Install packages only if not already installed
for (pkg in package_list) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

# load libraries and data
library(data.table)
library(ggplot2)
library(foreach)
library(pdftools)
library(magrittr)
library(gridExtra)

library(corrplot)
library(randomForest)
library(xgboost)

files <- list.files("data/")

loadTrain <- function() {
  train <- fread("data/train.csv")
  set.seed(123)
  validationRows <- 100
  train[, holdout_flag := 0
        ][sample(.N, round(.N * 0.3, 0)), holdout_flag := 1]
  train[(.N - validationRows) : .N, holdout_flag := -1]
  return(train)
}

dataSummary <- function(dt){
  dt[, .(column     = names(dt),
         num_zeros  = sapply(.SD, function(x) {sum(x == 0, na.rm = TRUE)}),
         prop_zeros = round(100 * sapply(.SD, function(x) {sum(x == 0, na.rm = TRUE) / .N}), 2),
         num_NA     = sapply(.SD, function(x) {sum(is.na(x))}),
         prop_zeros = round(100 * sapply(.SD, function(x) {sum(is.na(x)) / .N}), 2),
         type       = sapply(.SD, class),
         unique     = sapply(.SD, function(x) {uniqueN(x, na.rm = TRUE)})
  )]
}

featureGeneration <- function(dt){
  # dt[is.na(Age), Age := -1]
  dt[, Sex := as.factor(Sex)]
  
  dt[, male := ifelse(Sex == "male", 1, 0)]
  dt[, female := ifelse(Sex == "female", 1, 0)]
  dt[, response := as.factor(Survived)]
  
  dt[, Age :=  fifelse(is.na(Age), median(dt$Age, na.rm = TRUE), Age)]
  dt[, age_banded := round(Age / 10) * 10]
  
  return(dt)  
}

printTitle <- function(string) {
  cat("\n ############ \n ", string,
      "\n ############ \n")
  return(invisible(NULL))
}
