# Lorenzo Luciano
# May 18, 2014
# Test Titanic data on Kaggle

#set working directory
#setwd("C:/Users/llucia1/Google Drive/Kaggle/Titanic") # at work
setwd("C:/Users/lorenzol/Google Drive/Kaggle/Titanic") # at home

source("titanic_data_prep.R")

#install.packages('randomForest')
library(randomForest)
#install.packages('party')
library(party)

set.seed(415)

fit <- randomForest(as.factor(Survived) ~ Pclass
                    +Sex
                    +Age
                    +SibSp
                    +Parch
                    +Fare
                    +Cabin
                    +Embarked
                    +Title
                    +Child
                    +FamilySize,
                    data=train_m, controls=cforest_unbiased(ntree=2000, mtry=3))

varImpPlot(fit)

Prediction <- predict(fit, test_m, OOB=TRUE, type="response")

fit
summary(fit)



# Accuracy in Train
###################################################
pred_train <- predict(fit, train_m, OOB=TRUE, type="response")
train_confmat <- table(train$Survived, pred_train)
train_confmat
###################################################

# Prediction for test data
p1 <- predict(fit, test_m, OOB=TRUE, type="response")

results <- data.frame(PassengerId=test_m$PassengerId, Survived=p1)

write.csv(results, file="forest1.csv", row.names=F)

#Your submission scored 0.76077

anova(fit)
