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


# prediction-> everyone perished
test$Survived <- rep(0,418)

#create Kaggle submission dataset
submit <- data.frame(PassengerId = test$PassengerId, 
                     Survived = test$Survived)

write.csv(submit, file="allperish.csv", row.names=F)
