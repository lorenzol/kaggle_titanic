# Lorenzo Luciano
# May 20, 2014
# Titanic Competition on Kaggle using Random Forests

#set working directory
#setwd("C:/Users/llucia1/Google Drive/Kaggle/Titanic") # at work
setwd("C:/Users/lorenzol/Google Drive/Kaggle/Titanic") # at home

source("titanic_data_prep.R")


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
cv.ctrl <- trainControl(method = "repeatedcv", repeats = 3,
                        summaryFunction = twoClassSummary,
                        classProbs = TRUE)


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



# Model Evaluation
################################################
#install.packages("e1071")
library(e1071)

## Random Forest model
rf.pred <- predict(rf.tune, test.batch)
confusionMatrix(rf.pred, test.batch$Survived)


# ROC curves
################################################
## Random Forest model (RED curve)
rf.probs <- predict(rf.tune, test.batch, type = "prob")
rf.ROC <- roc(response = test.batch$Survived,
              predictor = rf.probs$Survived,
              levels = levels(test.batch$Survived))
plot(rf.ROC, add=TRUE, col="red") 
rf.ROC


# Create Submission File
########################################################

# use the model to generate predictions
Survived <- predict(rf.tune, newdata = test_m)

# reformat predictions to 0 or 1 and link to PassengerId in a data frame
Survived <- revalue(Survived, c("Survived" = 1, "Perished" = 0))
predictions <- as.data.frame(Survived)
predictions$PassengerId <- test_m$PassengerId

# write predictions to csv file for submission to Kaggle
write.csv(predictions[,c("PassengerId", "Survived")], 
          file="rf_tune_model1.csv", row.names=FALSE, quote=FALSE)


#May 20, 2014
#submission scored 0.80383