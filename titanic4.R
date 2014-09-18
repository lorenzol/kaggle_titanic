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

#add Survived varibale to test data with 'NA'
test$Survived <- NA

#combine train and test datasets
combi <- rbind(train,test)

#cast Name as a character instead of factor
combi$Name <- as.character(combi$Name)

#get title from Name
combi$Title <- sapply(combi$Name, 
                      FUN=function(x) 
                        {strsplit(x, split='[,.]')[[1]][2]})

#remove leading space from Title
combi$Title <- sub(' ', '', combi$Title)

combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'

combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'

combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

#change back to a factor
combi$Title <- factor(combi$Title)

#new FamilySixe variable
combi$FamilySize <- combi$SibSp + combi$Parch + 1

#extract surname
combi$Surname <- sapply(combi$Name, 
                        FUN=function(x) 
                          {strsplit(x, split='[,.]')[[1]][1]})

#combine familysize and surname into one variable
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")

#family of 2 or less is considered 'small'
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]

combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

train <- combi[1:892,]
test <- combi[892:1309,]

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train, method="class")

Prediction <- predict(fit, test, type="class")

#create Kaggle submission dataset
submit <- data.frame(PassengerId = test$PassengerId, 
                     Survived = Prediction)

write.csv(submit, file="decision_tree2.csv", row.names=F)
