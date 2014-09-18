# Lorenzo Luciano
# May 18, 2014
# Test Titanic data on Kaggle

#set working directory
#setwd("C:/Users/llucia1/Google Drive/Kaggle/Titanic") # at work
setwd("C:/Users/lorenzol/Google Drive/Kaggle/Titanic") # at home

source("titanic_data_prep.R")

library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)

fit <- rpart(Survived ~ Pclass 
             + Sex 
             + Age
             + SibSp
             + Parch
             + Cabin
             + Embarked
             + Title
             + Child
             + Fare2
             + FamilySize
             + FamilyID,
            data=train_m, method="class")

Prediction <- predict(fit, test_m, type="class")

fit
summary(fit)
rpart.plot(fit, type=4, extra=1)

#Fancy Plot
fancyRpartPlot(fit)

# Accuracy in Train
###################################################
pred_train <- predict(fit, train_m, type="class")
train_confmat <- table(train$Survived, pred_train)
train_confmat
###################################################

# Prediction for test data
p1 <- predict(fit, test_m, type="class")

results <- data.frame(PassengerId=test_m$PassengerId, Survived=p1)

write.csv(results, file="tree1.csv", row.names=F)



