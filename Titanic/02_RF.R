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

printTitle("RF")

train <- loadTrain()

## try random forest with Sex 
# iris.rf <- randomForest(Species ~ ., data=iris, importance=TRUE,
                        # proximity=TRUE)
# print(iris.rf)
# plot(iris)
# ggplot(iris) + geom_point(aes(x = Sepal.Length, y = Sepal.Width, col = Species))

train <- featureGeneration(train)

train.rf <- randomForest(response ~ age_banded + male + Pclass + Parch + SibSp, # Age + Sex, #+ Pclass + Fare, 
                         data = train[holdout_flag %in% c(0)],
                         ntree  = 100)
train[, prediction := predict(train.rf, .SD)]
err <- train[, .(.N, error = mean(prediction != response)), keyby = .(holdout_flag)]
print(err)

fwrite(train, "predictions_rf.csv")

# hist(train$prediction %>% as.numeric)
# summary(train$prediction)
# 
# train.rf$importance
# 
# par(mfrow = c(2,1))
# plot(train.rf$err.rate[,1], type = "l")
# plot(train.rf)
# 
# test <- featureGeneration(test)
# test[, prediction := predict(train.rf, .SD)]
# ?randomForest
# 
# ## Error by train and holdout
# # install.packages("caret")
# # library(caret)
# cm <- caret::confusionMatrix(train$prediction, train$response) 
# cm$overall
# ?confusionMatrix
# 
# caret::confusionMatrix(train[holdout_flag == 1, prediction], 
#                        train[holdout_flag == 1, response]) 
# caret::confusionMatrix(train[holdout_flag == 0, prediction], 
#                        train[holdout_flag == 0, response]) 
# caret::confusionMatrix(train$prediction, train$response) 
