##Housekeeping ####
rm(list=ls())
setwd('~\\Education\\CU\\MSBX 5415-001 Advanced Data Analytics\\Project')

library(e1071)
library(caret)
library(doSNOW)
library(ipred)
library(ranger)
library(dplyr)
library(vtreat)
library(magrittr)
library(xgboost)
library(ggplot2)
library(tree)
library(DiagrammeR)

traindata <- readRDS('traindatanodummies.rds')
testdata <- readRDS('testdatanodummies.rds')

#trees ####
set.seed(200)
t.basemod <- tree(avgCostPerMile~., data = traindata)
summary(t.basemod)

plot(t.basemod)
text(t.basemod,pretty=0)

cv.tbasemod <- cv.tree(t.basemod)
cv.tbasemod

plot(cv.tbasemod$size,cv.tbasemod$dev, type='b')

#prune?
prune.tbasemod <- prune.tree(t.basemod, best = 6)
plot(prune.tbasemod)
text(prune.tbasemod, pretty=0)

#check predictive abilities
yhat <- predict(t.basemod, newdata = testdata)
yhat <- predict(prune.tbasemod, newdata = testdata)
fare.test <- testdata[,'avgCostPerMile']

#plot(yhat,fare.test)
#abline(0,1)
#MSE
trRMSE <- sqrt(mean((yhat-fare.test)^2))

# Random Forest ####
fare_rf <- ranger(avgCostPerMile ~ .,
                  data = traindata,
                  num.trees = 100,
                  importance = 'impurity',
                  respect.unordered.factors = 'order')

boosted_rf <- ranger(avgCostPerMile ~ .,
                     data = traindata,
                     mtry = 38,
                     num.trees = 100,
                     importance = 'impurity',
                     respect.unordered.factors = 'order')

# Make predictions on the test data
testdata$pred <- predict(fare_rf, testdata)$predictions

# Calculate the RMSE of the predictions
resid <- testdata$avgCostPerMile-testdata$pred
rfRMSE <- sqrt(mean(resid^2))

# Plot actual outcome vs predictions (predictions on x-axis)
ggplot(testdata, aes(x = pred, y = avgCostPerMile)) +
    geom_point() +
    geom_abline()

plot(importance(fare_rf))

#xgBoost ####
# Hyperparameter optimization using cross validation in caret
# need to sample from full dataset to perform cross validation because pc does
# not have enough memory
sampfare <- sample(nrow(traindata), .05*nrow(traindata))
traindata <- traindata[sampfare,]

#train model
train.control <- trainControl(method = 'repeatedcv',
                              number = 10,
                              repeats = 3,
                              search = 'grid')

tune.grid <- expand.grid(eta = c(.05, .075, .1),
                         nrounds = c(100, 200, 300),
                         max_depth = 6:8,
                         min_child_weight = c(2.0, 2.25, 2.5),
                         colsample_bytree = c(.3, .4, .5),
                         gamma = 0,
                         subsample = 1)

cl <- makeCluster(4, type = 'SOCK')

registerDoSNOW(cl)

caret.cv <- train(avgCostPerMile ~.,
                  data = traindata,
                  method = 'xgbTree',
                  tuneGrid= tune.grid,
                  trControl = train.control)

stopCluster(cl)

caret.cv

#making predictions
preds <- predict(caret.cv, testdata)
#Evaluation using RMSE, R-Squared
postResample(preds, testdata$avgCostPerMile)

# Handling categoricals with vtreat
# Create and print a vector of variable names
vars <- colnames(select(traindata, -avgCostPerMile))

# Create the treatment plan
treatplan <- designTreatmentsZ(traindata, vars)

# Examine the scoreFrame
scoreFrame <- treatplan %>%
    use_series(scoreFrame) %>%
    select(varName, origName, code)

# We only want the rows with codes "clean" or "lev"
newvars <- scoreFrame %>%
    filter(code %in% c('clean', 'lev')) %>%
    use_series(varName)

# Create the treated training data
traindata.treat <- prepare(treatplan, traindata,
                           varRestriction = newvars)

# Use prepare() to one-hot-encode testframe
testdata.treat <- prepare(treatplan, testdata,
                          varRestriction = newvars)

# parameter optimization using xgb.cv
# Run xgb.cv
params <- list(booster = 'gbtree',
               objective = 'reg:linear',
               eta = .05,
               gamma = 0,
               max_depth = 6,
               min_child_weight = 2.25,
               subsample = 1,
               colsample_bytree = .4)

cv <- xgb.cv(data = as.matrix(traindata.treat), 
             label = traindata$avgCostPerMile,
             params = params,
             nrounds = 100,
             nfold = 10,
             showsd = TRUE,
             stratified = TRUE,
             print_every_n = 10,
             early_stopping_rounds = 10,
             maximize = FALSE
             )

#performance
min(cv$test.error.mean)

# Get the evaluation log
elog <- cv$evaluation_log

# Determine and print how many trees minimize training and test error
elog %>% 
    summarize(ntrees.train = which.min(train_rmse_mean),   # find the index of min(train_rmse_mean)
              ntrees.test  = which.min(test_rmse_mean))    # find the index of min(test_rmse_mean)
#ntrees.train = 97, ntrees.test = 87 - want least

min(elog$train_rmse_mean)
min(elog$test_rmse_mean)

#create the model
# Run xgboost
#first default - model training
ntrees = 90

fare_mod_xgb <- xgboost(data = as.matrix(traindata.treat),
                        label = traindata$avgCostPerMile,
                        nrounds = ntrees,
                        objective = 'reg:linear',
                        eta = .05, #.05
                        depth = 6, #6
                        gamma = 0, #0
                        min_child_weight = 2.25, #2.25
                        subsample = 1, #1
                        colsample_bytree = .4, #.4
                        lambda = 0, #0
                        alpha = 1, #1
                        verbose = 0  # silent
                        )

# Save the model
xgb.save(fare_mod_xgb, 'fare_mod_xgb')

# Make predictions
testdata$pred <- predict(fare_mod_xgb, as.matrix(testdata.treat))

# Calculate RMSE
xgbRMSE <- testdata %>%
                mutate(residuals = avgCostPerMile - pred) %>%
                summarize(rmse = sqrt(mean(residuals^2)))

#importance
mat <- xgb.importance(feature_names = colnames(traindata.treat),
                       model = fare_mod_xgb)
xgb.plot.importance(importance_matrix = mat[1:15])

# Plot predictions
ggplot(testdata, aes(x = pred, y = avgCostPerMile)) + 
    geom_point(aes(col = InvClass)) + 
    geom_abline() +
    xlim(c(0, 4)) +
    ylim(c(0, 4)) +
    labs(title = 'Actual vs. Predicted - By InvClass',
         y = 'Actual CPM', x = 'Predicted CPM')

ggplot(testdata, aes(x = pred, y = avgCostPerMile)) + 
    geom_point(aes(col = Channel)) + 
    geom_abline() +
    xlim(c(0, 4)) +
    ylim(c(0, 4)) +
    labs(title = 'Actual vs. Predicted - By Channel',
         y = 'Actual CPM', x = 'Predicted CPM')

xgb.plot.tree(feature_names = colnames(testdata.treat),
              model = fare_mod_xgb,
              trees = 8)

xgb.plot.multi.trees(model = fare_mod_xgb,
                     feature_names = colnames(testdata.treat),
                     features_keep = 5)
