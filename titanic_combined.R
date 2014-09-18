# Try combining then using a predition method
########################################################

# use the model to generate predictions
glm_pred_train <- as.data.frame(gpredict(glm.tune, newdata = train_m))
ctree_pred_train <- as.data.frame(predict(ctree.tune, newdata = train_m))
ada_pred_train <- as.data.frame(predict(ada.tune, newdata = train_m))
rf_pred_train <- as.data.frame(predict(rf.tune, newdata = train_m))
svm_pred_train <- as.data.frame(predict(svm.tune, newdata = train_m))


# combine all results in one dataframe
pred_comb <- cbind(glm_pred_train, ctree_pred_train, ada_pred_train, rf_pred_train, svm_pred_train)

colnames(pred_comb) <- c("glm","ctree","ada","rf","svm", "Survived")


library(caret)

set.seed(23)
training.rows <- createDataPartition(pred_comb$Survived, 
                                     p = 0.8, list = FALSE)
train.comb <- pred_comb[training.rows, ]
test.comb <- pred_comb[-training.rows, ]


#Modeling
#########################################################

#install.packages("pROC")
library(pROC)
## Define control function to handle optional arguments for train function
## Models to be assessed based on largest absolute area under ROC curve

cv.ctrl <- trainControl(method = "repeatedcv", # cross validation
                        number = 10,           # 10 fold 
                        repeats = 3,           # 3 seperate, 10 fold 
                        classProbs = TRUE,
                        summaryFunction = twoClassSummary
)

##########################################################
# glm model
set.seed(35)
glm.comb <- train(Survived ~ ctree + rf + svm,
                  data = train.comb,
                  method = "glm",
                  metric = "ROC",
                  trControl = cv.ctrl)

summary(glm.comb)

glm.pred <- predict(glm.comb, test.comb)
confusionMatrix(glm.pred, test.comb$Survived)


#Decision Tree
######################################################
ctree.comb <- train(Survived ~ glm + ctree + ada + rf + svm,
                    data = train.comb, 
                    method='ctree', 
                    tuneLength=5,
                    metric = "ROC",
                    trControl=cv.ctrl)

ctree.comb

ctree.pred <- predict(ctree.comb, test.comb)
confusionMatrix(ctree.pred, test.comb$Survived)

#RF
######################################################
rf.grid <- data.frame(.mtry = c(2, 3, 4, 5))
set.seed(35)
rf.comb <- train(Survived ~ glm + rf + ctree,
                 data = train.comb, 
                 method = "rf",
                 metric = "ROC",
                 tuneGrid = rf.grid,
                 trControl = cv.ctrl)
rf.comb

rf.pred <- predict(rf.comb, test.comb)
confusionMatrix(rf.pred, test.comb$Survived)

# SVM
#################################################

#install.packages("kernlab")
library(kernlab)
set.seed(35)
svm.comb <- train(Survived ~ glm + ada + ctree + rf + svm,
                  data = train.comb, 
                  method = "svmRadial",
                  tuneLength = 9, # try 9
                  preProcess = c("center", "scale"),
                  metric = "ROC",
                  trControl = cv.ctrl)

svm.tune

svm.pred <- predict(svm.tune, test.batch)
confusionMatrix(svm.pred, test.batch$Survived)

