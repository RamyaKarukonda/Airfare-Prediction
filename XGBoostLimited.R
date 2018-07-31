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

# Limit the variables
keepVars <- c('InvClass', 'Origin', 'Destination', 'Channel', 'avgCostPerMile',
              'avgLag', 'org_avg_inc_all_occ', 'dest_avg_inc_all_occ',
              'org_med_inc_all_occ', 'dest_med_inc_all_occ',
              'org_avg_inc_trans_occ', 'dest_avg_inc_trans_occ',
              'org_med_inc_trans_occ', 'dest_med_inc_trans_occ',
              'org_ttl_vcrimert', 'dest_ttl_vcrimert',
              'org_ttl_propcrimert', 'dest_ttl_propcrimert',
              'org_mpop05', 'dest_mpop05', 'org_mrmurd05',
              'dest_mrmurd05', 'org_mrrape05', 'dest_mrrape05', 'org_mrrobt05',
              'dest_mrrobt05', 'org_mrrobg05', 'dest_mrrobg05', 'org_mrassa05',
              'dest_mrassa05', 'org_mrassg05', 'dest_mrassg05', 'org_mrburg05',
              'dest_mrburg05', 'org_mrlarc05', 'dest_mrlarc05', 'org_mrmoto05',
              'dest_mrmoto05', 'wday')

keepVars <- c('InvClass', 'Channel', 'avgCostPerMile', 'avgLag',
              'org_avg_inc_all_occ', 'dest_avg_inc_all_occ',
              'org_mpop05', 'dest_mpop05', 'org_mrmurd05',
              'dest_mrmurd05', 'org_mrrape05', 'dest_mrrape05', 'org_mrrobt05',
              'dest_mrrobt05', 'org_mrrobg05', 'dest_mrrobg05', 'org_mrassa05',
              'dest_mrassa05', 'org_mrassg05', 'dest_mrassg05', 'org_mrburg05',
              'dest_mrburg05', 'org_mrlarc05', 'dest_mrlarc05', 'org_mrmoto05',
              'dest_mrmoto05', 'wday')

traindata.lmtd <- select(traindata, keepVars)
testdata.lmtd <- select(testdata, keepVars)

# Hyperparameter optimization using xgboost
# Handling categoricals with vtreat
# Create and print a vector of variable names
vars <- colnames(select(traindata.lmtd, -avgCostPerMile))

# Create the treatment plan
treatplan <- designTreatmentsZ(traindata.lmtd, vars)

# Examine the scoreFrame
scoreFrame <- treatplan %>%
    use_series(scoreFrame) %>%
    select(varName, origName, code)

# We only want the rows with codes "clean" or "lev"
newvars <- scoreFrame %>%
    filter(code %in% c('clean', 'lev')) %>%
    use_series(varName)

# Create the treated training data
traindata.treat <- prepare(treatplan, traindata.lmtd,
                           varRestriction = newvars)

# Use prepare() to one-hot-encode testframe
testdata.treat <- prepare(treatplan, testdata.lmtd,
                          varRestriction = newvars)

# parameter optimization using xgb.cv
# Run xgb.cv
params <- list(booster = 'gbtree',
               objective = 'reg:linear',
               eta = .1,
               gamma = 0,
               max_depth = 6,
               min_child_weight = 2.25,
               subsample = 1,
               colsample_bytree = .4)

cv <- xgb.cv(data = as.matrix(traindata.treat), 
             label = traindata.lmtd$avgCostPerMile,
             params = params,
             nrounds = 200,
             nfold = 10,
             showsd = TRUE,
             metrics = 'rmse',
             stratified = TRUE,
             print_every_n = 10,
             early_stopping_rounds = 10,
             maximize = FALSE
             )

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
ntrees = 100

fare_mod_xgb <- xgboost(data = as.matrix(traindata.treat),
                        label = traindata.lmtd$avgCostPerMile,
                        nrounds = ntrees,
                        objective = 'reg:linear',
                        eta = .3, #.3
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
#xgb.save(fare_mod_xgb, 'fare_mod_xgb')

# Make predictions
testdata.lmtd$pred <- predict(fare_mod_xgb, as.matrix(testdata.treat))

# Calculate RMSE
xgbRMSE <- testdata.lmtd %>%
                mutate(residuals = avgCostPerMile - pred) %>%
                summarize(rmse = sqrt(mean(residuals^2)))

#importance
mat <- xgb.importance(feature_names = colnames(traindata.treat),
                       model = fare_mod_xgb)
xgb.plot.importance(importance_matrix = mat[1:15])

# Plot predictions
ggplot(testdata.lmtd, aes(x = pred, y = avgCostPerMile)) + 
    geom_point(aes(col = InvClass)) + 
    geom_abline() +
    xlim(c(0, 4)) +
    ylim(c(0, 4)) +
    labs(title = 'Actual vs. Predicted - By InvClass',
         y = 'Actual CPM', x = 'Predicted CPM')

ggplot(testdata.lmtd, aes(x = pred, y = avgCostPerMile)) + 
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
