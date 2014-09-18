# Lorenzo Luciano
# May 17, 2014
# Test Titanic data on Kaggle

#set working directory
#setwd("C:/Users/llucia1/Google Drive/Kaggle/Titanic") # at work
setwd("C:/Users/lorenzol/Google Drive/Kaggle/Titanic") # at home

source("titanic_data_prep.R")


logit1 <- glm(Survived ~ Pclass + Sex + Age + SibSp + 
                         Parch + Fare + Cabin + Embarked, 
                data=train_m,
                family=binomial(link="logit"), na.action=na.pass)

#logit1 <- glm(Survived ~ Pclass + Sex + Age + Cabin, 
#             data=train_m,
#              family=binomial(link="logit"), na.action=na.pass)

logit1
summary(logit1)

predict1  <- predict(logit1, type="response")
summary(predict1)


###################################################
#Step 9: Plot the ROC Curve
###################################################
library(bitops)
library(caTools)
library(ROCR)

# this returns the probability scores on the training data
pred = predict(logit1, type="response") 

# prediction object needed by ROCR
predObj = prediction(pred, train_m$Survived) 

rocObj = performance(predObj, measure="tpr", x.measure="fpr")
aucObj = performance(predObj, measure="auc") 
auc = aucObj@y.values[[1]]  
auc  

# plot the roc curve
plot(rocObj, main = paste("Area under the curve:", auc))
###################################################

# Accuracy in Train
###################################################
test_train <- data.frame(pred)
test_train$pred[test_train$pred > 0.5] <- 1
test_train$pred[test_train$pred != 1] <- 0
test_train$Survived <- train_m$Survived

train_confmat <- table(test_train$Survived, test_train$pred)
train_confmat
den <- length(test_train$pred)
num <- length((subset(test_train,test_train$Survived == test_train$pred))$pred)
accuracy <- num/den
accuracy
###################################################

# Prediction for test data
p1 <- predict(logit1, newdata=test_m,type="response")


results <- cbind(PassengerId=test_m$PassengerId, Survived=p1)

results <- data.frame(results)
results$Survived[results$Survived > 0.5] <- 1
results$Survived[results$Survived != 1] <- 0


write.csv(results, file="logit1.csv", row.names=F)



