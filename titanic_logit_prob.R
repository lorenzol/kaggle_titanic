
summary(glm.tune)

glm.pred <- predict(glm.tune, test.batch)
confusionMatrix(glm.pred, test.batch$Survived)
glm.pred



p.train <- predict(glm.tune, train.batch, type="prob")
p.train <- cbind(p.train, actual=train.batch$Survived) 

p.test <- predict(glm.tune, test.batch, type="prob")
p.test <- cbind(p.test, actual=test.batch$Survived) 


predict(glm.tune, train.batch, type="prob")$Perished
predict(rf.tune, train.batch, type="prob")$Perished
predict(svm.tune, train.batch, type="prob")
predict(ada.tune, train.batch, type="prob")

logit1 <- glm(actual ~ Perished + Survived, 
              data=p.train,
              family=binomial(link="logit"), na.action=na.pass)

logit1
summary(logit1)


logit1.pred <- predict(logit1, p.test)
confusionMatrix(logit1.pred, p.test$actual)
logit1.pred
summary(logit1.pred)
dim(logit1.pred)


set.seed(35)
glm.tune <- train(actual ~ Perished + Survived, 
                  data=p.train,
                  method = "glm",
                  metric = "ROC",
                  trControl = cv.ctrl)

summary(glm.tune)

glm.pred <- predict(glm.tune, p.test)
confusionMatrix(glm.pred, p.test$actual)
glm.pred

p.test$predict <- "Survived"
p.test$predict[p.test$Perished + 0.2 > p.test$Survived]  <- "Perished"



dim(p.test[p.test$actual == p.test$predict,])
dim(p.test)
