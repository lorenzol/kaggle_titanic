# Lorenzo Luciano
# May 16, 2014
# Test Titanic data on Kaggle

#set working directory
setwd("C:/Users/llucia1/Google Drive/Kaggle/Titanic")

#import train dataset
train <- read.csv(
  "C:/Users/llucia1/Google Drive/Kaggle/Titanic/Data/train.csv")
#import test dataset
test <- read.csv(
  "C:/Users/llucia1/Google Drive/Kaggle/Titanic/Data/test.csv")


table(train$Survived)

table(train$Survived)
with(train, table(Pclass,Survived))
summary(train$Age)


logit1 <- glm(Survived ~ Pclass + Sex, data=train,
              family=binomial(link="logit"), na.action=na.pass)
logit1
summary(logit1)

predict1  <- predict(logit1, type="response")
predict1
summary(predict1)
table(train$Survived, predict1)


###################################################
#Step 9: Plot the ROC Curve
###################################################
install.packages("bitops")
library(bitops)
install.packages("caTools")
library(caTools)
install.packages("ROCR")
library(ROCR)

# this returns the probability scores on the training data
pred = predict(logit1, type="response") 

# prediction object needed by ROCR
predObj = prediction(pred, train$Survived) 

rocObj = performance(predObj, measure="tpr", x.measure="fpr")
aucObj = performance(predObj, measure="auc") 
auc = aucObj@y.values[[1]]  
auc  

# plot the roc curve
plot(rocObj, main = paste("Area under the curve:", auc))
###################################################


# Prediction for test data
p1 <- predict(logit1, newdata=test,type="response")


results <- cbind(PassengerId=test$PassengerId, Survived=p1)

results[results[,2] > 0.5,2] <- 1
results[results[,2] != 1, 2] <- 0
#or
results <- data.frame(results)
results$Survived[results$Survived > 0.5] <- 1
results$Survived[results$Survived != 1] <- 0


write.csv(results, file="simple_logit.csv", row.names=F)



