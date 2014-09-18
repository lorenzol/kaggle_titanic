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

library(rpart)


#create decision tree model
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train, method="class")

plot(fit)
text(fit)

#pakcages required for fancy plot
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)

#Fancy Plot
fancyRpartPlot(fit)

Prediction <- predict(fit, test, type="class")


#create Kaggle submission dataset
submit <- data.frame(PassengerId = test$PassengerId, 
                     Survived = Prediction)

write.csv(submit, file="decision_tree.csv", row.names=F)
