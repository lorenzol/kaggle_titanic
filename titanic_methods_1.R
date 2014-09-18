# Lorenzo Luciano
# May 19, 2014
# Titanic Competition on Kaggle

#set working directory
#setwd("C:/Users/llucia1/Google Drive/Kaggle/Titanic") # at work
setwd("C:/Users/lorenzol/Google Drive/Kaggle/Titanic") # at home

source("titanic_data_prep.R")

## map missing data by provided feature
require(Amelia)
#missmap(prep)


barplot(table(train_m$Pclass), 
        names.arg = c("first", "second", "third"),
        main="Pclass (passenger traveling class)", col="firebrick")

barplot(table(train_m$Sex), main="Sex (gender)", col="darkviolet")

hist(train_m$Age, main="Age", xlab = NULL, col="brown")

barplot(table(train_m$SibSp), main="SibSp (siblings + spouse aboard)", 
        col="darkblue")

barplot(table(train_m$Parch), main="Parch (parents + kids aboard)", 
        col="gray50")

hist(train_m$Fare, main="Fare (fee paid for ticket[s])", xlab = NULL, 
     col="darkgreen")

barplot(table(train_m$Embarked), 
        names.arg = c("Cherbourg", "Queenstown", "Southampton"),
        main="Embarked (port of embarkation)", col="sienna")


mosaicplot(train_m$Pclass ~ train_m$Survived, 
           main="Passenger Fate by Traveling Class", shade=FALSE, 
           color=TRUE, xlab="Pclass", ylab="Survived")

mosaicplot(train_m$Sex ~ train_m$Survived, 
           main="Passenger Fate by Gender", shade=FALSE, color=TRUE, 
           xlab="Sex", ylab="Survived")

boxplot(train_m$Age ~ train_m$Survived, 
        main="Passenger Fate by Age",
        xlab="Survived", ylab="Age")

mosaicplot(train_m$Embarked ~ train_m$Survived, 
           main="Passenger Fate by Port of Embarkation",
           shade=FALSE, color=TRUE, xlab="Embarked", ylab="Survived")


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



Titanic.logit.1 <- glm(Survived ~ Sex + Pclass + Age + FamilySize + Embarked + Fare, 
                       data = train.batch, family=binomial("logit"))

Titanic.logit.1                       
1-pchisq(308.1, df=9)

anova(Titanic.logit.1, test="Chisq")



Titanic.logit.2 <- glm(Survived ~ Sex + Pclass + Age + FamilySize + Embarked + Fare2, 
                       data = train.batch, family=binomial("logit"))

anova(Titanic.logit.2, test="Chisq")

glm(Survived ~ Sex + Pclass + Age + FamilySize + Embarked, 
    data = train.batch, family=binomial("logit"))


#Modeling
#########################################################

#install.packages("pROC")
library(pROC)
## Define control function to handle optional arguments for train function
## Models to be assessed based on largest absolute area under ROC curve
cv.ctrl <- trainControl(method = "repeatedcv", repeats = 3,
                        summaryFunction = twoClassSummary,
                        classProbs = TRUE)

#####################################################
# glm model 1
set.seed(35)
glm.tune.1 <- train(Survived ~ Sex + Pclass + Age + FamilySize + Embarked,
                    data = train.batch,
                    method = "glm",
                    metric = "ROC",
                    trControl = cv.ctrl)

glm.tune.1
summary(glm.tune.1)

#####################################################
# glm model 2
set.seed(35)
glm.tune.2 <- train(Survived ~ Sex + Pclass + Age + FamilySize + I(Embarked=="S"),
                    data = train.batch,
                    method = "glm",
                    metric = "ROC",
                    trControl = cv.ctrl)

summary(glm.tune.2)

#####################################################
# glm model 3
set.seed(35)
glm.tune.3 <- train(Survived ~ Sex + Pclass + Title + Age 
                    + FamilySize + I(Embarked=="S"),
                    data = train.batch,
                    method = "glm",
                    metric = "ROC",
                    trControl = cv.ctrl)

summary(glm.tune.3)

#####################################################
# glm model 4
set.seed(35)
glm.tune.4 <- train(Survived ~ Sex + Pclass
                    + I(Title=="Mr") + I(Title=="Mrs")
                    + I(Title=="Sir")
                    + Age + FamilySize + I(Embarked=="S"),
                    data = train.batch,
                    method = "glm",
                    metric = "ROC",
                    trControl = cv.ctrl)

summary(glm.tune.4)

# glm model 5
set.seed(35)
glm.tune.5 <- train(Survived ~ Sex + Pclass + Title + Age 
                    + FamilySize + I(Embarked=="S")
                    + I(Title=="Mr" & Pclass=="3"),
                    data = train.batch,
                    method = "glm",
                    metric = "ROC",
                    trControl = cv.ctrl)

summary(glm.tune.5)

##########################################################
# glm model 6
set.seed(35)
glm.tune.6 <- train(Survived ~ Pclass + Sex + Age + SibSp + Parch
                    + Fare + Cabin + Embarked + Title + Child
                    + Fare2 + FamilySize + FamilyID,
                    data = train.batch,
                    method = "glm",
                    metric = "ROC",
                    trControl = cv.ctrl)

summary(glm.tune.6)


# Other Models

#Boosted Classification Trees
######################################################
## note the dot preceding each variable
# .iter (number of boosting iterations, default=50)
# .maxdepth (depth of trees)
# .nu (shrinkage parameter, default=1)
ada.grid <- expand.grid(.iter = c(3, 6), #try(50,100)
                        .maxdepth = c(1, 2), #try(4,8)
                        .nu = c(1, 1)) # try(0.1,1)

set.seed(35)

#install.packages("ada")
library(ada)
ada.tune <- train(Survived ~ Pclass + Sex + Age + SibSp + Parch
                  + Cabin + Embarked + Title + Child
                  + FamilySize, 
                  data = train.batch,
                  method = "ada",
                  metric = "ROC",
                  tuneGrid = ada.grid,
                  trControl = cv.ctrl)

ada.tune
plot(ada.tune)


# Random Forest
#################################################
# Strobl et al suggested setting mtry at the square root of the number of variables. 
# In this case, that would be mtry = 2,
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
                  tuneLength = 6, # try 9
                  preProcess = c("center", "scale"),
                  metric = "ROC",
                  trControl = cv.ctrl)

svm.tune


# Model Evaluation
################################################

#install.packages("e1071")
library(e1071)

## Logistic regression model
glm.pred <- predict(glm.tune.6, test.batch)
confusionMatrix(glm.pred, test.batch$Survived)

## Boosted model
ada.pred <- predict(ada.tune, test.batch)
confusionMatrix(ada.pred, test.batch$Survived)

## Random Forest model
rf.pred <- predict(rf.tune, test.batch)
confusionMatrix(rf.pred, test.batch$Survived)

## SVM model 
svm.pred <- predict(svm.tune, test.batch)
confusionMatrix(svm.pred, test.batch$Survived)

# ROC curves
################################################
## Logistic regression model (BLACK curve)
glm.probs <- predict(glm.tune.6, test.batch, type = "prob")
glm.ROC <- roc(response = test.batch$Survived,
               predictor = glm.probs$Survived,
               levels = levels(test.batch$Survived))
plot(glm.ROC, type="S")
glm.ROC

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

cv.values <- resamples(list(Logit = glm.tune.6, Ada = ada.tune, 
                            RF = rf.tune, SVM = svm.tune))
dotplot(cv.values, metric = "ROC")


# Create Submission File
########################################################

# use the model to generate predictions
Survived <- predict(svm.tune, newdata = test_m)

# reformat predictions to 0 or 1 and link to PassengerId in a data frame
Survived <- revalue(Survived, c("Survived" = 1, "Perished" = 0))
predictions <- as.data.frame(Survived)
predictions$PassengerId <- test_m$PassengerId

# write predictions to csv file for submission to Kaggle
write.csv(predictions[,c("PassengerId", "Survived")], 
          file="svm_tune_model.csv", row.names=FALSE, quote=FALSE)


#May 19, 2014
#glm.tune.6->Your submission scored 0.78469
#ada.tune->Your submission scored 0.76077
#rf.tune->You improved on your best score by 0.00478 to 0.80383 
#SVM.tune->Your submission scored 0.79904