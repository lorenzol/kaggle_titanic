# Lorenzo Luciano
# May 20, 2014
# Titanic Competition on Kaggle

#set working directory
#setwd("C:/Users/llucia1/Google Drive/Kaggle/Titanic") # at work
setwd("C:/Users/lorenzol/Google Drive/Kaggle/Titanic") # at home

source("titanic_data_combine.R")

## map missing data by provided feature
#require(Amelia)
#missmap(prep)



#Fitting a Model
#######################################################

## split training data into train batch and test batch

#caret: Classification and Regression Training
#install.packages("caret")
library(caret)

set.seed(23)
training.rows <- createDataPartition(train_m$Survived, 
                                     p = 0.8, list = FALSE)
train.batch <- train_m[training.rows, ]
test.batch <- train_m[-training.rows, ]


#Modeling
#########################################################

#install.packages("pROC")
library(pROC)
## Define control function to handle optional arguments for train function
## Models to be assessed based on largest absolute area under ROC curve
## For Accuracy, uncomment classProbs = TRUE, and summaryFunction = twoClassSummary
cv.ctrl <- trainControl(#method = "repeatedcv",  # cross validation
                        method = "boot",       # choose 1 method
                        number = 10,            # 10 fold 
                        repeats = 3,            # 3 seperate 
                        returnResamp = "all",
                        #classProbs = TRUE,
                        #summaryFunction = twoClassSummary
                        )

##########################################################
# glm model
#Survived ~ Pclass + Sex + Age + SibSp + Parch
#+ I(Cabin=="E") + I(Embarked=="S") + I(Title=="Mr") + I(Child=="1")
#+ FamilySize,
set.seed(35)
glm.tune <- train(Survived ~ Pclass + Sex + Age + SibSp + Parch
                  + I(Ticket < 10) 
                  + Cabin + Embarked + Title + Child
                  + Fare_log + FamilySize,
                  data = train.batch,
                  method = "glm",
                  metric = "ROC",
                  tuneLength = 3, # try 9
                  trControl = cv.ctrl)

glm.tune
summary(glm.tune)

glm.pred <- predict(glm.tune, test.batch)
confusionMatrix(glm.pred, test.batch$Survived)

#Decision Tree
######################################################
ctree.tune <- train(Survived ~ Pclass + Sex + Age + SibSp + Parch
                    + Cabin + Embarked + Title + Child
                    + FamilySize, 
                    data = train.batch, 
                    method='ctree', 
                    tuneLength=5,
                    metric = "ROC",
                    trControl=cv.ctrl)

ctree.tune
plot(ctree.tune)



#Boosted Classification Trees
######################################################
## note the dot preceding each variable
# .iter (number of boosting iterations, default=50)
# .maxdepth (depth of trees)
# .nu (shrinkage parameter, default=1)
ada.grid <- expand.grid(.iter = c(5, 10), #try(50,100)
                        .maxdepth = c(2, 4), #try(4,8)
                        .nu = c(0.1, 1)) # try(0.1,1)

set.seed(35)

#install.packages("ada")
library(ada)
ada.tune <- train(Survived ~ Pclass + Sex + Age + SibSp + Parch
                  + Cabin + Embarked + Title + Child
                  + FamilySize, 
                  data = train.batch,
                  method = "ada",
                  metric = "ROC",
                  tuneGrid = ada.grid, # can be commented out
                  trControl = cv.ctrl)

ada.tune
plot(ada.tune)


# Random Forest
#################################################
# Strobl et al suggested setting mtry at the square root of the number of variables. 
# In this case, that would be mtry = 2,
#Survived ~ Pclass + Sex + Age + SibSp + Parch
#+ Cabin + Embarked + Title + Child
#+ FamilySize,
rf.grid <- data.frame(.mtry = c(2, 3))
set.seed(35)
rf.tune <- train(Survived ~ Pclass + Sex + Age + SibSp + Parch
                 + Cabin + Embarked + Title + Child
                 + FamilySize,
                 data = train.batch,
                 method = "rf",
                 metric = "ROC",
                 tuneGrid = rf.grid,
                 trControl = cv.ctrl)
rf.tune

rf.pred <- predict(rf.tune, test.batch)
confusionMatrix(rf.pred, test.batch$Survived)

# SVM
#################################################

#install.packages("kernlab")
library(kernlab)
set.seed(35)
svm.tune <- train(Survived ~ Pclass + Sex + Age + SibSp + Parch
                  + Cabin + Embarked + Title + Child
                  + FamilySize, 
                  data = train.batch,
                  method = "svmRadial",
                  tuneLength = 2, # try 9
                  preProcess = c("center", "scale"),
                  metric = "ROC",
                  trControl = cv.ctrl)

svm.tune

# KNN
#################################################
set.seed(35)
knn.tune <- train(Survived ~ Pclass + Sex + Age + SibSp + Parch
                  + Cabin + Embarked + Title + Child
                  + FamilySize, 
                  data = train.batch,
                  method = "knn",
                  # k=2, # not necessary
                  preProcess = c("center", "scale"),
                  tuneLength = 2, # try 10
                  metric = "ROC",
                  trControl = cv.ctrl)

knn.tune

knn.pred <- predict(knn.tune, test.batch)
confusionMatrix(knn.pred, test.batch$Survived)

# Avg NNET
#################################################
set.seed(825)
avnnet.tune <- train(Survived ~ Pclass + Sex + Age + SibSp + Parch
                     + Cabin + Embarked + Title + Child
                     + FamilySize, 
                     data = train.batch,
                     method = "avNNet",
                     trace = FALSE,
                     tuneLength = 2, # try 10
                     metric = "ROC",
                     trControl = cv.ctrl)

avnnet.tune

avnnet.pred <- predict(avnnet.tune, test.batch)
confusionMatrix(avnnet.pred, test.batch$Survived)

# GBM - Stochastic Gradient Boosting
#################################################
set.seed(825)
gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9),
                        n.trees = (1:30)*50,
                        shrinkage = 0.1)

nrow(gbmGrid)

gbm.tune <- train(Survived ~ Pclass + Sex + Age + SibSp + Parch
                  + Cabin + Embarked + Title + Child
                  + FamilySize, 
                  data = train.batch,
                  method = "gbm",
                  trControl = cv.ctrl,
                  verbose = FALSE
                  #tuneGrid = gbmGrid,#specify exact models to evaluate
                  #metric = "Accuracy"
                  ) 
gbm.tune

gbm.pred <- predict(gbm.tune, test.batch)
confusionMatrix(gbm.pred, test.batch$Survived)

trellis.par.set(caretTheme())
plot(gbm.tune)
ggplot(gbm.tune)

plot(gbm.tune, metric = "Accuracy")

# glmnet
#################################################
#Survived ~ Pclass + Sex + Age + SibSp + Parch
#+ Ticket + Cabin + Embarked + Title + Child
#+ Fare_log + FamilySize + FamilyID, 
#-> 100+55

#Survived ~ Pclass + Sex + Age + SibSp + Parch
#+ Ticket + Fare + Cabin + Embarked + Title + Child
#+ Fare_log + FamilySize + FamilyID,
#->101+54

glmnetGrid <- expand.grid(.alpha = (1:20) * 0.01, 
              .lambda = (1:20) * 0.01)

glmnet.tune <- train(Survived ~ Pclass + Sex + Age + SibSp + Parch
                     + Ticket + Fare + Cabin + Embarked + Title + Child
                     + Fare_log + FamilySize + FamilyID,
                     data = train.batch,
                     method = "glmnet",
                     tuneGrid = glmnetGrid,
                     trControl = cv.ctrl)

glmnet.tune

glmnet.pred <- predict(glmnet.tune, test.batch)
confusionMatrix(glmnet.pred, test.batch$Survived)

trellis.par.set(caretTheme())
plot(glmnet.tune)
ggplot(glmnet.tune)


# Model Evaluation
################################################

#install.packages("e1071")
library(e1071)

## Logistic regression model
glm.pred <- predict(glm.tune, test.batch)
confusionMatrix(glm.pred, test.batch$Survived)

## Decison Tree
ctree.pred <- predict(ctree.tune, test.batch)
confusionMatrix(ctree.pred, test.batch$Survived)

## Boosted model
ada.pred <- predict(ada.tune, test.batch)
confusionMatrix(ada.pred, test.batch$Survived)

## Random Forest model
rf.pred <- predict(rf.tune, test.batch)
confusionMatrix(rf.pred, test.batch$Survived)

## SVM model 
svm.pred <- predict(svm.tune, test.batch)
confusionMatrix(svm.pred, test.batch$Survived)

## KNN model 
knn.pred <- predict(knn.tune, test.batch)
confusionMatrix(knn.pred, test.batch$Survived)

# ROC curves
################################################
## Logistic regression model (BLACK curve)
glm.probs <- predict(glm.tune, test.batch, type = "prob")
glm.ROC <- roc(response = test.batch$Survived,
               predictor = glm.probs$Survived,
               levels = levels(test.batch$Survived))
plot(glm.ROC, type="S")

## Boosted model (GREEN curve)
ada.probs <- predict(ada.tune, test.batch, type = "prob")
ada.ROC <- roc(response = test.batch$Survived,
               predictor = ada.probs$Survived,
               levels = levels(test.batch$Survived))
plot(ada.ROC, add=TRUE, col="green")    
ada.ROC
## Random Forest model (RED curve)
rf.probs <- predict(rf.tune, test.batch, type = "prob")
rf.ROC <- roc(response = test.batch$Survived,
              predictor = rf.probs$Survived,
              levels = levels(test.batch$Survived))
plot(rf.ROC, add=TRUE, col="red") 
rf.ROC
## SVM model (BLUE curve)
svm.probs <- predict(svm.tune, test.batch, type = "prob")
svm.ROC <- roc(response = test.batch$Survived,
               predictor = svm.probs$Survived,
               levels = levels(test.batch$Survived))
plot(svm.ROC, add=TRUE, col="blue")
svm.ROC

# graph which sums up the performance of the four models
########################################################

cv.values <- resamples(list(Logit = glm.tune, ctree= ctree.tune,
                            Ada = ada.tune, 
                            RF = rf.tune, SVM = svm.tune))
dotplot(cv.values, metric = "ROC")


# Create Submission File
########################################################

# use the model to generate predictions
Survived_glm <- predict(glm.tune, newdata = test_m)
Survived_ctree <- predict(ctree.tune, newdata = test_m)
Survived_ada <- predict(ada.tune, newdata = test_m)
Survived_rf <- predict(rf.tune, newdata = test_m)
Survived_svm <- predict(svm.tune, newdata = test_m)
Survived_glmnet <- predict(glmnet.tune, newdata = test_m)

# reformat predictions to 0 or 1
Survived_glm <- revalue(Survived_glm, c("Survived" = 1, "Perished" = 0))
Survived_ctree <- revalue(Survived_ctree, c("Survived" = 1, "Perished" = 0))
Survived_ada <- revalue(Survived_ada, c("Survived" = 1, "Perished" = 0))
Survived_rf <- revalue(Survived_rf, c("Survived" = 1, "Perished" = 0))
Survived_svm <- revalue(Survived_svm, c("Survived" = 1, "Perished" = 0))
Survived_glmnet <- revalue(Survived_glmnet, c("Survived" = 1, "Perished" = 0))

# in a data frame
pred_glm <- as.data.frame(Survived_glm)
pred_ctree <- as.data.frame(Survived_ctree)
pred_ada <- as.data.frame(Survived_ada)
pred_rf <- as.data.frame(Survived_rf)
pred_svm <- as.data.frame(Survived_svm)
pred_glmnet <- as.data.frame(Survived_glmnet)

# results dataframe
#predictions <- cbind(pred_glm, pred_ctree, pred_ada, pred_rf, pred_svm)
predictions <- pred_glmnet

# add Passenger ID
predictions$PassengerId <- test_m$PassengerId


# write predictions to csv file for submission to Kaggle
write.csv(predictions[,c("PassengerId", "Survived_glm")], 
          file="glm_tune_model6.csv", row.names=FALSE, quote=FALSE)

write.csv(predictions[,c("PassengerId", "Survived_ctree")], 
          file="ctree_tune_model6.csv", row.names=FALSE, quote=FALSE)

write.csv(predictions[,c("PassengerId", "Survived_ada")], 
          file="ada_tune_model6.csv", row.names=FALSE, quote=FALSE)

write.csv(predictions[,c("PassengerId", "Survived_rf")], 
          file="rf_tune_model6.csv", row.names=FALSE, quote=FALSE)

write.csv(predictions[,c("PassengerId", "Survived_svm")], 
          file="svm_tune_model6.csv", row.names=FALSE, quote=FALSE)


write.csv(predictions[,c("PassengerId", "Survived_glmnet")], 
          file="glmnet_model3.csv", row.names=FALSE, quote=FALSE)


