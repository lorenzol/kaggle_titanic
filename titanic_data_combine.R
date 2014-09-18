# Lorenzo Luciano
# May 17, 2014
# Test Titanic data on Kaggle

#set working directory
#setwd("C:/Users/llucia1/Google Drive/Kaggle/Titanic") # at work
setwd("C:/Users/lorenzol/Google Drive/Kaggle/Titanic") # at home


#import train and test dataset
train <- read.csv("./Data/train.csv")
test <- read.csv("./Data/test.csv")

#combine train and test for data prep
prep <- rbind(train[c(1,3:12)], test)

# Amelia - to view ans analyze missing data
######################################
#install.packages("Amelia")
library(Amelia)
#AmeliaView()
## map missing data by provided feature
#missmap(prep)


# Survived Variable ( change to a factor)
########################################
train$Survived <- as.factor(train$Survived)
#install.packages("plyr")
require(plyr) # for the revalue function 
train$Survived <- revalue(train$Survived, c("1" = "Survived", "0" = "Perished"))

#pclass variable
######################################
table(prep$Pclass) 

barplot(table(train$Pclass), 
        names.arg = c("first", "second", "third"),
        main="Pclass (passenger traveling class)", col="firebrick")

mosaicplot(train$Pclass ~ train$Survived, 
           main="Passenger Fate by Traveling Class", shade=FALSE, 
           color=TRUE, xlab="Pclass", ylab="Survived")

prep[is.na(prep$Pclass)] # no nulls
#change Pclass to factor type
prep$Pclass <- as.factor(prep$Pclass)


#Name
######################################
prep[is.na(prep$Name),]
#do not use for now

#Title - extract title from Name
######################################
prep$Name <-  as.character(prep$Name) # change name to character type
prep$Title <- sapply(prep$Name, FUN=function(x)  # extract title from name
                        {strsplit(x, split='[,.]')[[1]][2]})
prep$Title <- sub(' ', '', prep$Title) # remove leading space

prep$Title[prep$Title == "Mlle"] <- "Ms" # Mlle is Ms in french
prep$Title[prep$Title == "Mme"] <- "Mrs" # Mme is Mrs in french

#group Title with low numbers
prep$Title[prep$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
prep$Title[prep$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'


prep$Title <- as.factor(prep$Title)
table(prep$Title)

#Sex
######################################
barplot(table(prep$Sex), main="Sex (gender)", col="darkviolet")

mosaicplot(train$Sex ~ train$Survived, 
           main="Passenger Fate by Gender", shade=FALSE, color=TRUE, 
           xlab="Sex", ylab="Survived")

table(prep$Sex) #female(466), male(843)
table(prep$Sex)/length(prep$Sex) #female(0.356), male(0.644)

#Age
######################################
hist(prep$Age, main="Age", xlab = NULL, col="brown")

boxplot(train$Age ~ train$Survived, 
        main="Passenger Fate by Age",
        xlab="Survived", ylab="Age")

prep$Age <- as.integer(prep$Age)
summary(prep$Age) #median(28) mean(29.88) NA's(263)

#try changing missing age to median age
#prep$Age[is.na(prep$Age)] <- median(prep$Age, na.rm=TRUE)

#median age by Title ( to fill in blank ages)
age_title <- aggregate(Age ~ Title, data=prep, FUN=median,na.action=na.exclude) 

#temp dataframe for age
age_temp <- merge(x=prep, y=age_title, by="Title", all.x=TRUE)

prep$Age[is.na(prep$Age)] <- age_temp$Age.y[is.na(age_temp$Age.x)]

prep$Age <- as.integer(prep$Age)

#Cild
######################################
prep$Child <- 0
prep$Child[prep$Age < 18] <- 1
prep$Child <- as.factor(prep$Child)

#SibSp
######################################
barplot(table(prep$SibSp), main="SibSp (siblings + spouse aboard)", 
        col="darkblue")

table(prep$SibSp)
subset(prep, is.na(SibSp)) # no nulls

#Parch
######################################
barplot(table(prep$Parch), main="Parch (parents + kids aboard)", 
        col="gray50")
table(prep$Parch)
subset(prep, is.na(Parch)) # no nulls

#Ticket
######################################

train$Ticket <- as.factor(substr(train$Ticket,1,1))

table(train$Ticket, train$Survived)

barplot(table(prep$Ticket), main="Ticket",col="gray50")

mosaicplot(train$Ticket ~ train$Survived, 
           main="Passenger Fate by Ticket",
           shade=FALSE, color=TRUE, xlab="Ticket", ylab="Survived")

prep$Ticket <- as.factor(substr(prep$Ticket,1,1))

#Fare
######################################
#numeric
hist(prep$Fare, main="Fare (fee paid for ticket[s])", xlab = NULL, 
     col="darkgreen")
summary(prep$Fare) # median(14.450),mean(33.300), NA's(1)
boxplot(prep$Fare)

prep$Fare[is.na(prep$Fare)] <- median(prep$Fare, na.rm=TRUE)

prep$Fare_log <- prep$Fare
prep$Fare_log[prep$Fare != 0] <- log10(prep$Fare[prep$Fare != 0])

#Fare2
######################################
prep$Fare2 <- '30+'
prep$Fare2[prep$Fare < 30 & prep$Fare >= 20]  <- '20-30'
prep$Fare2[prep$Fare < 20 & prep$Fare >= 10] <- '10-20'
prep$Fare2[prep$Fare < 10] <- '<10'
prep$Fare2 <- as.factor(prep$Fare2)


#Cabin
######################################
# not marked as NA, use == "", instead
# passengers with a cabin had a survival rate of 67%, otherwise 30% so good marker
# changed to a factor, "" factor plus first letter of cabin
# try also 2 first letters of Cabin, maybe 3?
prep$Cabin <- as.factor(substr(prep$Cabin,1,1))

#Embarked
######################################
barplot(table(prep$Embarked), 
        names.arg = c("NA", "Cherbourg", "Queenstown", "Southampton"),
        main="Embarked (port of embarkation)", col="sienna")


mosaicplot(train$Embarked ~ train$Survived, 
           main="Passenger Fate by Port of Embarkation",
           shade=FALSE, color=TRUE, xlab="Embarked", ylab="Survived")


prep$Embarked
table(train$Embarked, train$Survived)

#Replace 2 missing values with "S"
prep$Embarked <- as.character(prep$Embarked)
prep$Embarked[prep$Embarked == ""] <- "S"
prep$Embarked <- as.factor(prep$Embarked)



#FamilySize
######################################
prep$FamilySize <- prep$SibSp + prep$Parch + 1

prep$FamilySize <- as.integer(prep$FamilySize)

table(prep$FamilySize)



#FamilyID
######################################
prep$Surname <- sapply(prep$Name, 
                        FUN=function(x) 
                        {strsplit(x, split='[,.]')[[1]][1]})

prep$FamilyID <- paste(as.character(prep$FamilySize), prep$Surname, sep="")

prep$FamilyID[prep$FamilySize <= 2] <- 'Small'

famIDs <- data.frame(table(prep$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]

prep$FamilyID[prep$FamilyID %in% famIDs$Var1] <- 'Small'
prep$FamilyID <- factor(prep$FamilyID)


#Create data.frame for Model
#################################################################
train_m <- prep[1:891,] #split data between train and test
train_m <- cbind(train_m, Survived=train$Survived) # add Survived to train

test_m <- prep[892:1309,] # test data for model




