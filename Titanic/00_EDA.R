###############################
## Titanic EDA
## Lachlan Bubb
## 04/17/2020
###############################
rm(list = ls());

# setwd("~/Titanic/")

source("99_functions.R")

# Goals
# - Set up a pipeline to create feature, train model, score + diagnose
# - Add Logicistic regression, RF, XGBoost (any others) model types
# - Iterate to build a decent model
# - Attempt stacking RF + XGBOOST models
# - CV Folds? Parameter tuning

# Load data
files <- list.files("data/")
train <- fread("data/train.csv")
test  <- fread("data/test.csv")

# set random holdout sample (30% of rows)
train[, holdout_flag := 0
      ][sample(.N, round(.N * 0.3, 0)), holdout_flag := 1]

train[, .N, by = holdout_flag]
train[holdout_flag == 1]

dim(train)
str(train)
dataSummary(train)
summary(train)

# EDA NOTES
# All fields complete except cabin is missing data?
# Sex is correlated
# Pclass is correlated
# SibSp, Parch - definition? looks correlated
# Age looks correlated but needs investigation 

##  EDA 
# Field - response 
p <- foreach(k = names(train)) %do%{
  ggplot(train) + geom_bar(aes_string(x = k)) + facet_wrap(Survived~.)
}

pdf(file = "testPlots.pdf")
marrangeGrob(grobs = p, nrow = 2, ncol = 2)
dev.off()

train[, age_banded := round(Age / 10) * 10]
ggplot(train) + geom_bar(aes(x = age_banded)) + facet_wrap(Survived~.)
ggplot(train) + geom_bar(aes(x = SibSp)) + facet_wrap(Survived~.)
ggplot(train) + geom_bar(aes(x = family_size)) + facet_wrap(Survived~.)



# Correlations 
numericCols <- names(train)[train[, sapply(.SD, class) != "character"]]
dt   <- train[, numericCols, with = FALSE]
dt %>% pairs
dt %>% cor

correlations <- dt[!is.na(Age), !c("holdout_flag")] %>% cor
corrplot(correlations, 
         type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
