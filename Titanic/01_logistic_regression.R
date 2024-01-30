###############################
## Titanic RF
## Lachlan Bubb
## 04/17/2020
###############################
rm(list = ls()); gc();

# setwd("~/Titanic/")

source("99_functions.R")

# Goals
# - Set up a pipeline to create feature, train model, score + diagnose
# - Add Logicistic regression, RF, XGBoost (any others) model types
# - Iterate to build a decent model
# - Attempt stacking RF + XGBOOST models
# - CV Folds? Parameter tuning

# set random holdout sample (30% of rows)
# train + test split in 70/30 split (0/1)
# Take last 100 rows as validation sample (-1)

printTitle("Logistic")

train <- loadTrain()
train[, .N, by = holdout_flag]

train <- featureGeneration(train)

model <- glm(response ~ age_banded + male + Pclass + Parch + SibSp,
             family = binomial(link = 'logit'), 
             data   = train[holdout_flag != -1])

train[, prediction := predict(model, .SD, type = "response")]
err <- train[, .(.N, error = mean(as.numeric(prediction > 0.5) != response)), keyby = .(holdout_flag)]
print(err)

fwrite(train, "predictions_logistic.csv")