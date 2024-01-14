###############################
## Titanic RF
## Lachlan Bubb
## 04/17/2020
###############################
rm(list = ls()); gc();

setwd("~/kaggle/Titanic/")

source("99_functions.R")

# Goals
# - Set up a pipeline to create feature, train model, score + diagnose
# - Add Logicistic regression, RF, XGBoost (any others) model types
# - Iterate to build a decent model
# - Attempt stacking RF + XGBOOST models

printTitle("XGB-Logistic stacking")

train <- fread("predictions_logistic.csv")
train[, prediction_log := prediction]

features <- c("age_banded", "male", "Pclass", "Parch", "SibSp", "prediction_log")

dtrain <- xgb.DMatrix(data = data.matrix(train[holdout_flag == 0, ..features]), 
                      label = data.matrix(train[holdout_flag == 0, response]))
dholdout <- xgb.DMatrix(data = data.matrix(train[holdout_flag == 1, ..features]), 
                        label = data.matrix(train[holdout_flag == 1, response]))

# Set up other params
baseScore <- train[holdout_flag == 0, mean(Survived)] # starting loc of 

# objective function, booster + treemethod?
params <- list(
  booster     = "gbtree", # Use gbtree for classification? Else use default
  eta         = 0.05,     # Learning rate - use between 0.01 - 0.05
  objective   = "binary:logistic", # for classification, can use Binary: logistic, Multi:softmax, Multi:softprob
  max_depth   = 2,       # the maximum depth of each decision tree
  base_score  = baseScore,  # initial prediction - can speed up convergence
  subsample   = 1,        # Prop of rows xgb will use to train tree
  eval_metric = "error"    # Evaluation metric (error is default for classification, RMSE for regression)
) 

xgb <- xgb.train(data                  = dtrain,     # Training data
                 params                = params,
                 nrounds               = 1000,       # max number of boosting iterations
                 early_stopping_rounds = 100,         # Stop if no improvement for X rounds
                 verbose               = FALSE,          # Print out results
                 print_every_n         = 10,
                 watchlist             = list(train   = dtrain, # Set train + holdout sets
                                              holdout = dholdout))

train[, prediction := predict(xgb, data.matrix(.SD[, ..features]))]
err <- train[, .(.N, error = mean(as.numeric(prediction > 0.5) != response)), keyby = .(holdout_flag)]
print(err)

# XGB diagnostics
# xgb.importance(model = xgb)
# xgb.plot.deepness(model = xgb)
# xgb.plot.importance(importance_matrix = xgb.importance(model = xgb))
# xgb.plot.shap(data  = data.matrix(train[holdout_flag == 0, ..features]),
#               model = xgb)
# xgb.plot.tree(data  = data.matrix(train[holdout_flag == 0, .(Age, male)]), 
#               model = xgb)

# Old attempt
# treeMethod <- "exact"
# objectiveFunction <-  "reg:logistic" #"count:poisson"
# eta <- 0.05
# depth <- 2
# subSample <- 1
# minChildWeight <- 1
# seed <- 1223
# nThread <- 2
# stoppingRounds <- 100
# numTrees <- 100
# 
# watchList <- list(train = dtrain, holdout = dholdout)
# 
# xgb <- xgb.train(data = dtrain,
#                  tree_method = treeMethod, 
#                  objective = objectiveFunction,
#                  eta = eta,
#                  max_depth = depth,
#                  subsample = subSample,
#                  min_child_weight = minChildWeight,
#                  colsample_bytree = 1, 
#                  nthread = nThread,
#                  base_score = baseScore,
#                  nrounds = numTrees,
#                  early_stopping_rounds = stoppingRounds,
#                  verbose = 1,
#                  maximize = FALSE,
#                  print_every_n = 10,
#                  watchlist = watchList)
# 
# train[, prediction := predict(xgb, data.matrix(.SD[, .(Age, male)]))]
# hist(train$prediction)
# summary(train$prediction)
# summary(train$response)
# 
# plot(train$prediction, train$response)
# 
# ggplot(train, aes(x= prediction, y=response)) + geom_point() + 
#   stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)

