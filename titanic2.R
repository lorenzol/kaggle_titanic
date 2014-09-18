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

summary(train$Sex)

#proportion of female and men that survived
prop.table(table(train$Sex, train$Survived),1)

test$Survived <- 0
#mark females as having survived
test$Survived[test$Sex == 'female'] <- 1

#create Kaggle submission dataset
submit <- data.frame(PassengerId = test$PassengerId, 
                     Survived = test$Survived)

write.csv(submit, file="females_survive.csv", row.names=F)


summary(train$Age)

#create new variable Child(if under 18)
train$Child <- 0
train$Child[train$Age < 18] <- 1

aggregate(Survived ~ Child + Sex, data=train, FUN=sum) 

aggregate(Survived ~ Child + Sex, data=train, FUN=length)

aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) 
                                {sum(x)/length(x)})


#bin fares into less thab $10, $10-$20, $20-$30, more than $30
train$Fare2 <- '30+'
train$fare2[train$Fare < 30 & train$Fare >= 20]  <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

aggregate(Survived ~ Fare2 + Pclass + Sex, 
          data=train, 
          FUN=function(x) {sum(x)/length(x)})

test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0

#create Kaggle submission dataset
submit <- data.frame(PassengerId = test$PassengerId, 
                     Survived = test$Survived)

write.csv(submit, file="females_class_survive.csv", row.names=F)
