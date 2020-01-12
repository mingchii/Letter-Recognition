library(dplyr)
library(ggplot2)
library(caTools)
library(ROCR)
library(MASS)
library(rpart) # CART 
library(rpart.plot) # CART plotting
library(caret) # cross validation
library(randomForest)
library(gbm)


Letters <- read.csv("Letters.csv")
Letters$isB<- as.factor(Letters$letter=="B")
set.seed(456)
train.ids<-sample(nrow(Letters),0.65*nrow(Letters))
Letters.train<- Letters[train.ids,]
Letters.test<- Letters[-train.ids,]

#Accuracy of baseline 
table(Letters.test$isB==FALSE)

#logistic regression
mod <- glm(isB ~ xbox + ybox + width + height + 
             onpix + xbar + ybar + x2bar + y2bar + 
             xybar + x2ybar + xy2bar + xedge + xedgeycor + yedge + yedgexcor
           , data=letters.train, family="binomial")

summary(mod)

# Predictions on the test set 
predTest = predict(mod, newdata=letters.test, type="response")
summary(predTest)
# Now, create the confusion matrix with threshold probability = 0.5.
table(letters.test$isB, predTest > 0.5)


#AUC of the logistic regression model
rocr.log.pred <- prediction(predTest, letters.test$isB)
as.numeric(performance(rocr.log.pred, "auc")@y.values)

#cart model

cpvalue <- data.frame(cp=seq(0,0.1, by=0.002))
set.seed(456)
cartmodel<- train(isB ~ xbox + ybox + width + height + 
                    onpix + xbar + ybar + x2bar + y2bar + 
                    xybar + x2ybar + xy2bar + xedge + xedgeycor + yedge + yedgexcor
                  , data=letters.train,
                  method="rpart",
                  tuneGrid=cpvalue,
                  trControl=trainControl(method = "cv",number=10),
                  metric="Accuracy")
cartmodel
bestcart <- cartmodel$finalModel
ggplot(cartmodel$results,aes(x=cp,y=Accuracy))+geom_point(size=1)+
  xlab("Complexity Parameter(cp)"+geom_line())

letters.test.mm <- as.data.frame(model.matrix(isB~.+0,data=letters.test[2:18]))
cart.predict <- predict(bestcart,newdata=letters.test.mm,type="class")
table(letters.test$isB,cart.predict)

#random forests

mod.rf <- randomForest(isB ~ xbox + ybox + width + height + 
                         onpix + xbar + ybar + x2bar + y2bar + 
                         xybar + x2ybar + xy2bar + xedge + xedgeycor + yedge + yedgexcor
                       , data = letters.train,method = "rf", metric = "Accuracy", mtry = 5, nodesize = 5, ntree = 500)

pred.rf <- predict(mod.rf, newdata = letters.test)
table(letters.test$isB,pred.rf)


#Question B
Letters <- read.csv("Letters.csv")
set.seed(456)

train.ids<-sample(nrow(Letters),0.65*nrow(Letters))
Letters.train<- Letters[train.ids,]
Letters.test<- Letters[-train.ids,]

table(Letters.train$letter)
table(Letters.test$letter,Letters.test$letter=='A')

#LDA model
LDAmodel <-lda(letter~xbox + ybox + width + height + 
                 onpix + xbar + ybar + x2bar + y2bar + 
                 xybar + x2ybar + xy2bar + xedge + xedgeycor + yedge + yedgexcor
               ,data=Letters.train)
predictLDA <-predict(LDAmodel,newdata=Letters.test)
predictLDA.probab <- predictLDA$posterior
lettername <- c("A","B","P","R")
predictclass <- lettername[max.col(predictLDA.probab)]
table(Letters.test$letter,predictclass)



#CART model

cpvalue <- data.frame(cp=seq(0,0.1, by=0.002))
set.seed(456)
cartmodelQ2<- train(letter ~ xbox + ybox + width + height + 
                    onpix + xbar + ybar + x2bar + y2bar + 
                    xybar + x2ybar + xy2bar + xedge + xedgeycor + yedge + yedgexcor
                  , data=Letters.train,
                  method="rpart",
                  tuneGrid=cpvalue,
                  trControl=trainControl(method = "cv",number=10),
                  metric="Accuracy")
cartmodelQ2
bestcartQ2 <- cartmodelQ2$finalModel
ggplot(cartmodelQ2$results,aes(x=cp,y=Accuracy))+geom_point(size=1)+
  xlab("Complexity Parameter(cp)"+geom_line())
prp(bestcartQ2)

Letters.test.mm <- as.data.frame(model.matrix(letter~.+0,data=Letters.test))
cart.predictQ2 <- predict(bestcartQ2,newdata=Letters.test.mm,type="class")
table(Letters.test$letter,cart.predictQ2)


#bagging
Letters.train.mm <- as.data.frame(model.matrix(letter~.+0,data=Letters.train))
Baggingmodel <-randomForest(x=Letters.train.mm,y=Letters.train$letter,mtry=16,
                            nodesize = 5,ntree=500)
Bagging.predict <- predict(Baggingmodel,newdata=Letters.test.mm)
table(Letters.test$letter,Bagging.predict) 

#random forest

RFmodelQ2 <- train(letter ~ xbox + ybox + width + height + 
                         onpix + xbar + ybar + x2bar + y2bar + 
                         xybar + x2ybar + xy2bar + xedge + xedgeycor + yedge + yedgexcor
                       , data = Letters.train,method = "rf", 
                       tunegrid = data.frame(mtry=1:16),
                       trControl=trainControl(method = "cv", number = 10,
                       verboselter=TRUE),metric="Accuracy")

RFmodelQ2$results
RFmodelQ2
bestRFQ2 <- RFmodelQ2$finalModel

RF.bestpredictQ2 <- predict(bestRFQ2, newdata = Letters.test.mm)
table(Letters.test$letter,RF.bestpredictQ2)

ggplot(RFmodelQ2$results,aes(x=mtry,y=Accuracy))
+geom_point(size=3)+ylab("Accuracy")+theme_bw()+theme(axis.title = element_text(size=18),
                                                      axis.text = element_text(size=18))


##boosting model
boostingmodel<-gbm(letter~.,
                   data=Letters.train,
                   distribution = "multinomial",
                   n.tree=3300,
                   interation.depth=10)
boosting.predict <- predict(boostingmodel,newdata=Letters.test,n.trees=3300,type="response")
boosting.predict<- boosting.predict%>%as.data.frame()%>%as.matrix()
boosting.predict.class<-factor(max.col(boosting.predict),levels=c(1,2,3,4),labels=c("A","B","P","R"))

table(Letters.test$letter,boosting.predict.class)





