library(dplyr)

predictTitanic <- function(){
    rawTrain <- read.csv("~/Kaggle/Titanic/train.csv")
    rawTest <- read.csv("~/Kaggle/Titanic/test.csv")
    
    train <- cleanTitanicFrame(rawTrain)
    test <- cleanTitanicFrame(rawTest)
    
    
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

    
    #TODO: analyze why failures failed
    #rawTrain[(train$Survived!=train$projSurvived),]
    
    
    #Correlations show that best predictors for Survival are Sex, PClass, Fare, Embarked
    #Sex and Pclass are strongest. Fare is very highly correlated with Pclass.
    #Maybe use Embarked, but it seems weird to matter in and of itself.
    
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
    
    ctree = ctree(as.factor(Survived)~Sex+Pclass+SibSp+projAge, data=train)
    test <- mutate(test,Survived=predict(ctree,newdata=test))
    
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
    
    #drop foctors with no categorical meaning
    frame[,!names(frame) %in% c("PassengerId","Name","Ticket","Cabin")]
    
}