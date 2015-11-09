library(dplyr)
library(caret)
library(adabag)

predictTitanic <- function(){
    rawTrain <- read.csv("~/Kaggle/Titanic/train.csv")
    rawTest <- read.csv("~/Kaggle/Titanic/test.csv")
    
    train <- cleanTitanicFrame(rawTrain)
    test <- cleanTitanicFrame(rawTest)
    
#     FOR EVALUATING PERFORMANCE ON TRAINING DATA    
#     #Evaluate percentage of train set correct
#     model <- glm("Survived~Sex+Pclass",family=binomial,data=train)
#     train <- mutate(train,rawLogit=predict(model))
#     train <- mutate(train,prob=1/(1+exp(-train$rawLogit)))
#     train <- mutate(train,projSurvived=ifelse(train$prob>.5,1,0))
#     
#     #Through exploratory analysis, it is apparent that almost no one under 18 in Pclass 1 or 2 died.
#     train$projSurvived=ifelse((train$Pclass!=3)&(train$projAge<16),1,train$projSurvived)
#     
#     #Through exploratory analysis, it is apparent that poor children with siblings were more likely to die.
#     train$projSurvived=ifelse((train$Parch>0)&(train$SibSp>0)&(train$Pclass==3),0,train$projSurvived)
#     
#     sum(train$Survived==train$projSurvived)/length(train$Survived)

    
#     LOGISTIC REGRESSION ON TEST DATA    
#     #Evaluate model and translate to 0/1
#     model <- glm("Survived~Sex+Pclass",family=binomial,data=train)
#     test <- mutate(test,rawLogit=predict(model,newdata = test))
#     test <- mutate(test,prob=1/(1+exp(-test$rawLogit)))
#     test <- mutate(test,Survived=ifelse(test$prob>.5,1,0))
# 
#     #Through exploratory analysis, it is apparent that almost no one under 18 in Pclass 1 or 2 died.
#     test$Survived=ifelse((test$Pclass!=3)&(test$projAge<18),1,test$Survived)
#     
#     #Through exploratory analysis, it is apparent that poor children with siblings were more likely to die.
#     test$Survived=ifelse((test$Parch>0)&(test$SibSp>2)&(test$Pclass==3),0,test$Survived)
    

# #   LOGISTIC MODEL TREE ON TEST DATA        
#     mob <- mob(Survived~ SibSp + Parch + Embarked | Sex + Pclass + projAge, data=train,family=binomial("logit"))
#     test <- mutate(test,rawMob=predict(mob,newdata=test))
#     test <- mutate(test,Survived=ifelse(test$rawMob>.5,1,0))
    
#   ADABOOST ON TEST DATA
    trainAdaboost <- mutate(train,Survived=as.factor(Survived))
    adaboost <- boosting(Survived~Sex+Pclass+SibSp+projAge, data=trainAdaboost)
    p <- predict(adaboost,newdata = test)
    test$Survived <- p$class
    
    #Write submission to file
    submission <- cbind(rawTest["PassengerId"],test["Survived"])
    write.csv(submission,"~/Kaggle/Titanic/RSexClass.csv",row.names=FALSE)
    
}

cleanTitanicFrame <- function(frame){
    #make factors numeric when categorical
    frame[,"Sex"] <- as.numeric(frame[,"Sex"])
    frame[,"Embarked"] <- as.numeric(frame[,"Embarked"])
    
    #Failed attempt at translating cabin to numeric. Doesn't correlate.
    #frame[,"Cabin"] <- sapply(frame[,"Cabin"],function(x){ifelse(x!="",as.numeric(charToRaw(substr(x,1,1)))-65,NA)})
    
    #Account for missing age values. Correlations show that best predictors are
    #Pclass, SibSp, and Parch are best predictors
    model <- glm("Age~SibSp+Parch+Pclass",data=frame)
    frame <- mutate(frame,projAge=coef(model)[1]+coef(model)[2]*frame$SibSp+coef(model)[3]*frame$Parch+coef(model)[4]*frame$Pclass)
    frame <- mutate(frame,projAge=ifelse(is.na(frame$Age),frame$projAge,frame$Age))
    
    #Account for missing fare values
    frame$Fare[is.na(frame$Fare)] <- median(frame$Fare,na.rm=TRUE)
    
    #drop foctors with no categorical meaning
    frame[,!names(frame) %in% c("PassengerId","Name","Ticket","Cabin")]
    
}

ensemblePrediction <- function(){
    rawTrain <- read.csv("~/Kaggle/Titanic/train.csv")
    rawTest <- read.csv("~/Kaggle/Titanic/test.csv")
    
    train <- cleanTitanicFrame(rawTrain)
    test <- cleanTitanicFrame(rawTest)
    
#     inTrain = createDataPartition(train$Survived, p = 2/3)[[1]]
#     train1 = train[ inTrain,]
#     train2 = train[-inTrain,]
    
    #Train using random forest, boosting, linear discriminant analysis, and logistic regression
    forest <- train(as.factor(Survived)~Sex+Pclass+SibSp+projAge+Parch+Fare+Embarked,method="rf",data=train)
    gboost <- train(as.factor(Survived)~Sex+Pclass+SibSp+projAge+Parch+Fare+Embarked,method="gbm",data=train,verbose=FALSE)
    logitTree <- train(as.factor(Survived)~Sex+Pclass+SibSp+projAge+Parch+Fare+Embarked,method="LMT",data=train)
    lda <- train(as.factor(Survived)~Sex+Pclass+projAge+SibSp+Parch+Fare+Embarked,method="lda",data=train)
    xgb <- train(as.factor(Survived)~Sex+Pclass+projAge+SibSp+Parch+Fare+Embarked,method="xgbLinear",data=train)
    
    #CHOOSE BEST MODELS
#     pred1 <- predict(forest,train2)
#     pred2 <- predict(gboost,train2)
#     pred3 <- predict(logitTree,train2)
#     pred4 <- predict(lda,train2)
#     pred5 <- predict(xgb,train2)
#     
#     #stack predictions and compare
#     predDF <- data.frame(pred1,pred2,pred3,pred4,pred5,Survived=train2$Survived)
#     #combModelFit <- train(as.factor(Survived)~.,method="rf",data=predDF)
#     #combPred <- predict(combModelFit,predDF)
#     #predDF$comb <- combPred
#     predDF$vote <- round(rowMeans(data.matrix(predDF[,which(colnames(predDF) != "Survived")])))-1
#     caret::confusionMatrix(predDF$pred1,predDF$Survived)$overall
#     caret::confusionMatrix(predDF$pred2,predDF$Survived)$overall
#     caret::confusionMatrix(predDF$pred3,predDF$Survived)$overall
#     caret::confusionMatrix(predDF$pred4,predDF$Survived)$overall
#     caret::confusionMatrix(predDF$pred5,predDF$Survived)$overall
#     caret::confusionMatrix(predDF$vote,predDF$Survived)$overall
    
    pred1 <- predict(forest,test)
    pred2 <- predict(gboost,test)
    pred3 <- predict(logitTree,test)
    pred4 <- predict(lda,test)
    pred5 <- predict(xgb,test)
    
    #Stack predictions and compare
    predDF <- data.frame(pred1,pred2,pred3,pred4,pred5)
    test$Survived <- round(rowMeans(data.matrix(predDF)))-1
    
    #Write submission to file
    submission <- cbind(rawTest["PassengerId"],test["Survived"])
    write.csv(submission,"~/Kaggle/Titanic/REnsemble.csv",row.names=FALSE)
    
}
