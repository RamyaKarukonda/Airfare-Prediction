##Housekeeping ####
rm(list=ls())
setwd('~\\Education\\CU\\MSBX 5415-001 Advanced Data Analytics\\Project')
require(data.table)
require(ggplot2)
require(purrr)
require(dplyr)
require(tidyverse)
require(Hmisc)
require(corrplot)
library(caret)

##  read in required files ####
traindata <- readRDS('traindatanodummies.rds')
testdata <- readRDS('testdatanodummies.rds')

#scale the data
ind <- sapply(traindata, is.numeric)
traindata[ind] <- lapply(traindata[ind], scale)
testdata[ind] <- lapply(testdata[ind], scale)

#linear models ####
#lm1 - base model without Origin or Destination airport codes
modTrainData <- select(traindata, -Origin, -Destination)
modTestData <- select(testdata, -Origin, -Destination)

lm_base <- lm(avgCostPerMile ~., data = modTrainData)

summary(lm_base)

preds <- predict(lm_base, newdata = modTestData, type = 'response')
lm1RMSE <- RMSE(modTestData$avgCostPerMile, preds)

#lm2 - using average income - all occupations, and individual crime stats
keepVars <- c('InvClass', 'Channel', 'avgCostPerMile', 'avgLag',
              'org_avg_inc_all_occ', 'dest_avg_inc_all_occ',
              'org_mpop05', 'dest_mpop05', 'org_mrmurd05', 'dest_mrmurd05',
              'org_mrrape05', 'dest_mrrape05', 'org_mrrobt05',
              'dest_mrrobt05', 'org_mrrobg05', 'dest_mrrobg05',
              'org_mrassa05', 'dest_mrassa05', 'org_mrassg05',
              'dest_mrassg05', 'org_mrburg05', 'dest_mrburg05',
              'org_mrlarc05', 'dest_mrlarc05', 'org_mrmoto05',
              'dest_mrmoto05', 'wday')

modTrainData <- select(traindata, keepVars)
modTestData <- select(testdata, keepVars)

lm_avgIncAllOccAllCrime <- lm(avgCostPerMile ~., data = modTrainData)

summary(lm_avgIncAllOccAllCrime)
preds <- predict(lm_avgIncAllOccAllCrime,
                 newdata = modTestData, type = 'response')
lm2RMSE <- RMSE(modTestData$avgCostPerMile, preds)

#lm3 - using median income - all occupations, and individual crime stats
keepVars <- c('InvClass', 'Channel', 'avgCostPerMile', 'avgLag',
              'org_med_inc_all_occ', 'dest_med_inc_all_occ',
              'org_mpop05', 'dest_mpop05', 'org_mrmurd05', 'dest_mrmurd05',
              'org_mrrape05', 'dest_mrrape05', 'org_mrrobt05',
              'dest_mrrobt05', 'org_mrrobg05', 'dest_mrrobg05',
              'org_mrassa05', 'dest_mrassa05', 'org_mrassg05',
              'dest_mrassg05', 'org_mrburg05', 'dest_mrburg05',
              'org_mrlarc05', 'dest_mrlarc05', 'org_mrmoto05',
              'dest_mrmoto05', 'wday')

modTrainData <- select(traindata, keepVars)
modTestData <- select(testdata, keepVars)

lm_medIncAllOccAllCrime <- lm(avgCostPerMile ~., data = modTrainData)

summary(lm_medIncAllOccAllCrime)
preds <- predict(lm_medIncAllOccAllCrime,
                 newdata = modTestData, type = 'response')
lm3RMSE <- RMSE(modTestData$avgCostPerMile, preds)

#lm4 - using avg income - trans occupations, and indivudal crime stats
keepVars <- c('InvClass', 'Channel', 'avgCostPerMile', 'avgLag',
              'org_avg_inc_trans_occ', 'dest_avg_inc_trans_occ',
              'org_mpop05', 'dest_mpop05', 'org_mrmurd05', 'dest_mrmurd05',
              'org_mrrape05', 'dest_mrrape05', 'org_mrrobt05',
              'dest_mrrobt05', 'org_mrrobg05', 'dest_mrrobg05',
              'org_mrassa05', 'dest_mrassa05', 'org_mrassg05',
              'dest_mrassg05', 'org_mrburg05', 'dest_mrburg05',
              'org_mrlarc05', 'dest_mrlarc05', 'org_mrmoto05',
              'dest_mrmoto05', 'wday')

modTrainData <- select(traindata, keepVars)
modTestData <- select(testdata, keepVars)

lm_avgIncTransOccAllCrime <- lm(avgCostPerMile ~., data = modTrainData)

summary(lm_avgIncTransOccAllCrime)
preds <- predict(lm_avgIncTransOccAllCrime,
                 newdata = modTestData, type = 'response')
lm4RMSE <- RMSE(modTestData$avgCostPerMile, preds)

#lm5 - using med income - trans occupations, and individual crime stats
keepVars <- c('InvClass', 'Channel', 'avgCostPerMile', 'avgLag',
              'org_med_inc_trans_occ', 'dest_med_inc_trans_occ',
              'org_mpop05', 'dest_mpop05', 'org_mrmurd05', 'dest_mrmurd05',
              'org_mrrape05', 'dest_mrrape05', 'org_mrrobt05',
              'dest_mrrobt05', 'org_mrrobg05', 'dest_mrrobg05',
              'org_mrassa05', 'dest_mrassa05', 'org_mrassg05',
              'dest_mrassg05', 'org_mrburg05', 'dest_mrburg05',
              'org_mrlarc05', 'dest_mrlarc05', 'org_mrmoto05',
              'dest_mrmoto05', 'wday')

modTrainData <- select(traindata, keepVars)
modTestData <- select(testdata, keepVars)

lm_medIncTransOccAllCrime <- lm(avgCostPerMile ~., data = modTrainData)

summary(lm_medIncTransOccAllCrime)
preds <- predict(lm_medIncTransOccAllCrime,
                 newdata = modTestData, type = 'response')
lm5RMSE <- RMSE(modTestData$avgCostPerMile, preds)

#lm6 - using avg income - all occupations, and total crime stats
keepVars <- c('InvClass', 'Channel', 'avgCostPerMile', 'avgLag',
              'org_avg_inc_all_occ', 'dest_avg_inc_all_occ',
              'org_mpop05', 'dest_mpop05', 'org_ttl_vcrimert',
              'dest_ttl_vcrimert', 'org_ttl_propcrimert',
              'dest_ttl_propcrimert', 'wday')

modTrainData <- select(traindata, keepVars)
modTestData <- select(testdata, keepVars)

lm_avgIncAllOccTtlCrime <- lm(avgCostPerMile ~., data = modTrainData)

summary(lm_avgIncAllOccTtlCrime)
preds <- predict(lm_avgIncAllOccTtlCrime,
                 newdata = modTestData, type = 'response')
lm6RMSE <- RMSE(modTestData$avgCostPerMile, preds)

#lm7 - using med income - all occupations, and total crime stats
keepVars <- c('InvClass', 'Channel', 'avgCostPerMile', 'avgLag',
              'org_med_inc_all_occ', 'dest_med_inc_all_occ',
              'org_mpop05', 'dest_mpop05', 'org_ttl_vcrimert',
              'dest_ttl_vcrimert', 'org_ttl_propcrimert',
              'dest_ttl_propcrimert', 'wday')

modTrainData <- select(traindata, keepVars)
modTestData <- select(testdata, keepVars)

lm_medIncAllOccTtlCrime <- lm(avgCostPerMile ~., data = modTrainData)

summary(lm_medIncAllOccTtlCrime)
preds <- predict(lm_medIncAllOccTtlCrime,
                 newdata = modTestData, type = 'response')
lm7RMSE <- RMSE(modTestData$avgCostPerMile, preds)

#lm8 - using avg income - trans occupations, and total crime stats
keepVars <- c('InvClass', 'Channel', 'avgCostPerMile', 'avgLag',
              'org_avg_inc_trans_occ', 'dest_avg_inc_trans_occ',
              'org_mpop05', 'dest_mpop05', 'org_ttl_vcrimert',
              'dest_ttl_vcrimert', 'org_ttl_propcrimert',
              'dest_ttl_propcrimert', 'wday')

modTrainData <- select(traindata, keepVars)
modTestData <- select(testdata, keepVars)

lm_avgIncTransOccTtlCrime <- lm(avgCostPerMile ~., data = modTrainData)

summary(lm_avgIncTransOccTtlCrime)
preds <- predict(lm_avgIncTransOccTtlCrime,
                 newdata = modTestData, type = 'response')
lm8RMSE <- RMSE(modTestData$avgCostPerMile, preds)

#lm9 - using med income - trans occupations, and total crime stats
keepVars <- c('InvClass', 'Channel', 'avgCostPerMile', 'avgLag',
              'org_med_inc_trans_occ', 'dest_med_inc_trans_occ',
              'org_mpop05', 'dest_mpop05', 'org_ttl_vcrimert',
              'dest_ttl_vcrimert', 'org_ttl_propcrimert',
              'dest_ttl_propcrimert', 'wday')

modTrainData <- select(traindata, keepVars)
modTestData <- select(testdata, keepVars)

lm_medIncTransOccTtlCrime <- lm(avgCostPerMile ~., data = modTrainData)

summary(lm_medIncTransOccTtlCrime)
preds <- predict(lm_medIncTransOccTtlCrime,
                 newdata = modTestData, type = 'response')
lm9RMSE <- RMSE(modTestData$avgCostPerMile, preds)

# PCA ####
#Running PCA on data with dummy variables
##  read in required files ####
rm(list = ls())
traindata <- readRDS('traindata.rds')
testdata <- readRDS('testdata.rds')

traindata_PCA = prcomp(traindata[,-1],scale=TRUE)

#This plot shows that the first 15 vectors covers most of the variation
plot(summary(traindata_PCA)$importance[3,], xlab = 'Principle Component',
     ylab = 'Proportion of Variance')

#limit the linear model to just those principle components
trainPCA = cbind(avgCostPerMile = traindata$avgCostPerMile,
                       data.frame(traindata_PCA$x[,cumsum(summary(traindata_PCA)$importance[2,])<.95]))

lm_PCA <- lm(avgCostPerMile ~., data = trainPCA)

summary(lm_PCA)
preds <- predict(lm_PCA, type = 'response')

lm10RMSE <- RMSE(traindata$avgCostPerMile, preds)

#Variable Analysis ####
alias(lm_PCA)
cols = sapply(modTrainData,is.numeric)
mcor <- round(cor(modTrainData[,cols]),2)
upper <- mcor
upper[upper.tri(mcor)] <- ""
corrMatrix <- as.data.frame(upper)

corrplot(mcor[1:20,1:20], method = 'ellipse', type = 'lower')
corrplot(mcor[21:35,21:35], method = 'ellipse', type = 'lower')
corrplot(mcor[41:56,41:56], method = 'ellipse', type = 'lower')

#pairs() #pairwise scatterplots
#Finding correlation and removing columns that have cor > 0.8
hc = findCorrelation(mcor, cutoff=0.8, names = TRUE, verbose = TRUE)
hc = sort(hc)

par(mfrow=c(2,2))
plot(lm_PCA)
par(mfrow=c(1,1))
