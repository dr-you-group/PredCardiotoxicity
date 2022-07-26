library(dplyr)
library(tidyverse)
library(caret)
library(randomForest)
library(themis)
library(xgboost)
library(ggplot2)

## set seed number
seedNum = 1234

## set proportion for the test set
testProportion = 0.3

#### Function to use ####
minmaxNormalize <- function(x) {return ((x-min(x,na.rm = T)) / (max(x,na.rm = T)-min(x,na.rm = T)))}
zNormalize <- function(x) {return ( (x-mean(x,na.rm = T))/sd(x,na.rm = T))}

#### Set the outcome ####
patients$cardiotoxicity <- ifelse(patients$cardiotoxicity == 1, "yes", "no")
positiveClass <- "yes"
outcomeName = "cardiotoxicity" #The name of column
outcome <- patients %>% pull(outcomeName) %>% factor(levels = c("no", "yes"))

#### Set variables ####
catVar <- c("cancerType",
            "chemoType",
            "cancerStage",
            "HTN",
            "DM",
            "Dyslipidemia",
            "CAOD",
            "aFib",
            "CKD",
            "RTx")

numVar <- c("Age",
            "baseEF",
            "baseLvGls",
            "deltaEF",
            "deltaLvGls")

patientsCat <- patients %>% select(all_of(catVar))
patientsNum <- patients %>% select(all_of(numVar))
#patientsNumR <- as.data.frame(lapply(patientsNum, zNormalize))

#### One hot encoding ####
for(col in colnames(patientsCat)){
  patientsCat[,col] <- as.character(patientsCat[,col] )
}

dummy <- caret::dummyVars("~ .", data = patientsCat) 
patientsCatD <- data.frame(predict(dummy, newdata = patientsCat))

feature <- cbind(patientsCatD, patientsNum)

#### Split train and test ####
set.seed(seedNum)
testInd <- caret::createDataPartition(outcome, p = testProportion, list = F)

featureTrain <- feature[-testInd,]
featureTest  <- feature[testInd,]

outcomeTrain <- outcome[-testInd]
outcomeTest <- outcome[testInd]

## Check distribution of test and train data ##
prop.table(table(outcomeTrain)) * 100
prop.table(table(outcomeTest)) * 100


#### Using trainControl in caret package ####

fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 5,
                           classProbs = TRUE, summaryFunction = twoClassSummary,
                           search = "grid",savePredictions = T)

fitControl$sampling <- "smote" # "rose", "smote"

#### Random Forest ####

set.seed(seedNum)

tunegrid <- expand.grid(.mtry=c(1:ncol(feature)))
trainFit <- train(x = featureTrain, y = outcomeTrain,
                  method = "rf",
                  tuneGrid = tunegrid,
                  trControl = fitControl,
                  metric = "ROC")

trainFit

trainFit$finalModel

# performance on CV resample data 
getTrainPerf(trainFit)

confusionMatrix(trainFit, "none")

##### Evaluation #####
predictClass <- predict(trainFit,newdata = featureTest)
predictProb <- predict(trainFit,newdata = featureTest,type ="prob")

#confusion matrix
caret::confusionMatrix(predictClass, outcomeTest,positive = positiveClass)

#the accuracy
mean(predictClass == outcomeTest) 

# Roc curve
Epi::ROC(form=outcomeTest~predictProb[,2], plot="ROC")
out= Epi::ROC(form=outcomeTest~predictProb[,2], plot="ROC")
out$AUC 

# variable importance plot 
plot(varImp(trainFit))

#### XGb in caret####
fitControlXgb <- trainControl(method = "repeatedcv", number = 5, repeats = 5,
                              classProbs = TRUE, summaryFunction = caret::twoClassSummary)

fitControlXgb$sampling <- "smote" 

customGrid <- expand.grid(
  nrounds = c(seq(from=200, to = 1000, by = 50)),
  eta = c(0.01, 0.05, 0.1, 0.3),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)


set.seed(seedNum)
fitXgb <- caret::train(x = featureTrain, 
                       y = outcomeTrain,
                       method = "xgbTree",
                       tuneGrid = customGrid,
                       trControl = fitControlXgb,
                       metric = "ROC",
                       verbosity = 0)

fitXgb

##### Step 2 #####

customGrid_2 <- expand.grid(
  nrounds = c(seq(from=50, to = 1000, by = 50)),
  eta = fitXgb$bestTune$eta,
  max_depth = ifelse(fitXgb$bestTune$max_depth == 2, c(fitXgb$bestTune$max_depth:4),
                     fitXgb$bestTune$max_depth - 1 : fitXgb$bestTune$max_depth + 1),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = c(1,2,3,4,5),
  subsample = 1
)



set.seed(seedNum)
fitXgb_2 <- caret::train(x = featureTrain, 
                         y = outcomeTrain,
                         method = "xgbTree",
                         tuneGrid = customGrid_2,
                         trControl = fitControlXgb,
                         metric = "ROC",
                         verbosity = 0)

fitXgb_2

##### Step 3 #####
customGrid_3 <- expand.grid(
  nrounds = c(seq(from=50, to = 1000, by = 50)),
  eta = fitXgb$bestTune$eta,
  max_depth = fitXgb_2$bestTune$max_depth,
  gamma = 0,
  colsample_bytree = c(seq(0.6,1.0,0.1)),
  min_child_weight = fitXgb_2$bestTune$min_child_weight,
  subsample = c(0.6,0.75,0.8,0.9,1.0)
)



set.seed(seedNum)
fitXgb_3 <- caret::train(x = featureTrain, 
                         y = outcomeTrain,
                         method = "xgbTree",
                         tuneGrid = customGrid_3,
                         trControl = fitControlXgb,
                         metric = "ROC",
                         verbosity = 0)

fitXgb_3

##### Step 4 #####
customGrid_4 <- expand.grid(
  nrounds = c(seq(from=50, to = 1000, by = 50)),
  eta = fitXgb$bestTune$eta,
  max_depth = fitXgb_2$bestTune$max_depth,
  gamma = c(0,0.05,0.1,0.5,0.7,0.9,1.0),
  colsample_bytree = fitXgb_3$bestTune$colsample_bytree,
  min_child_weight = fitXgb_2$bestTune$min_child_weight,
  subsample = fitXgb_3$bestTune$subsample
)



set.seed(seedNum)
fitXgb_4 <- caret::train(x = featureTrain, 
                         y = outcomeTrain,
                         method = "xgbTree",
                         tuneGrid = customGrid_4,
                         trControl = fitControlXgb,
                         metric = "ROC",
                         verbosity = 0)

fitXgb_4

##### Step 5 #####
customGrid_5 <- expand.grid(
  nrounds = c(50, seq(from=100, to = 10000, by = 100)),
  eta = c(0.01,0.015,0.025,0.05,0.01),
  max_depth = fitXgb_2$bestTune$max_depth,
  gamma = fitXgb_4$bestTune$gamma,
  colsample_bytree = fitXgb_3$bestTune$colsample_bytree,
  min_child_weight = fitXgb_2$bestTune$min_child_weight,
  subsample = fitXgb_3$bestTune$subsample
)



set.seed(seedNum)
fitXgb_5 <- caret::train(x = featureTrain, 
                         y = outcomeTrain,
                         method = "xgbTree",
                         tuneGrid = customGrid_5,
                         trControl = fitControlXgb,
                         metric = "ROC",
                         verbosity = 0)

fitXgb_5

##### Step 6 #####
fitControlXgb <- trainControl(method = "repeatedcv", number = 5, repeats = 5,
                              classProbs = TRUE, summaryFunction = caret::twoClassSummary)

fitControlXgb$sampling <- "smote" 

finalGrid <- expand.grid(
  nrounds = fitXgb_5$bestTune$nrounds,
  eta = fitXgb_5$bestTune$eta,
  max_depth = fitXgb_5$bestTune$max_depth,
  gamma = fitXgb_5$bestTune$gamma,
  colsample_bytree = fitXgb_5$bestTune$colsample_bytree,
  min_child_weight = fitXgb_5$bestTune$min_child_weight,
  subsample = fitXgb_5$bestTune$subsample
)



set.seed(seedNum)
fitXgbFinal <- caret::train(x = featureTrain, 
                            y = outcomeTrain,
                            method = "xgbTree",
                            tuneGrid = finalGrid,
                            trControl = fitControlXgb,
                            metric = "ROC",
                            verbosity = 0)

fitXgbFinal

# performance on CV resample data 
getTrainPerf(fitXgbFinal)

confusionMatrix(fitXgbFinal, "none")

#### Evaluation ####
predictClass <- predict(fitXgbFinal,newdata = featureTest)
predictProb <- predict(fitXgbFinal,newdata = featureTest,type ="prob")

#confusion matrix
caret::confusionMatrix(predictClass, outcomeTest,positive = positiveClass)

#the accuracy
mean(predictClass == outcomeTest) 


# Roc curve
Epi::ROC(form=outcomeTest~predictProb[,2], plot="ROC")
out= Epi::ROC(form=outcomeTest~predictProb[,2], plot="ROC")
out$AUC 

# variable importance plot
plot(varImp(fitXgbFinal))

#### Change cut-off #### 
optimal_lr.eta=function(x){
  no=which.max(x$res$sens+x$res$spec)[1]
  result=x$res$lr.eta[no]
  result
}

optimalEta <- optimal_lr.eta(out) 


b0 <- unname(out$lr$coeff[1])
b1 <- unname(out$lr$coeff[2])
threshold = (-log(1/optimalEta-1)-b0)/b1

predOutcomeProb <- predict(fitXgbFinal, newdata = featureTest, type = "prob" )[,2]
predCutoff <- as.factor(ifelse(predOutcomeProb > threshold, "yes", "no"))

caret::confusionMatrix(predCutoff, outcomeTest, positive = positiveClass)