library(dplyr)


meanDigits <- function(){
    
    if(!exists("digitTrain")) digitTrain <- read.csv("~/Kaggle/Digits/train.csv")
    if(!exists("digitTest")) digitTest <- read.csv("~/Kaggle/Digits/test.csv")
    
    #Build prototypical digits from training data
    if(!exists("meanDigitFrame")){
        meanDigits <- matrix(nrow=0,ncol=785)
        for (digit in seq(0,9,1)){
            meanPixels <- sapply(digitTrain[digitTrain$label==digit,],mean)
            meanDigits <- rbind(meanDigits,meanPixels)
        }
        
        meanDigitFrame <- as.data.frame(meanDigits)
        row.names(meanDigitFrame) <- NULL
    }
    
    #Label each test digit image by comparison with the prototypical digits
    ImageId <- seq(28000)
    Label <- apply(digitTest,1,digitDiff)
    prediction <- data.frame(ImageId,Label)
    #Write submission to file
    write.csv(prediction,"~/Kaggle/Digits/MeanPixels.csv",row.names = FALSE)
}

digitDiff <- function(digitLine){
    # Finds the sum of the square of the residuals of pixels for each prototypical digit
    #
    # Args:
    #   digitLine: Vector of pixel information for a given digit
    # Returns:
    #   "Average" digit with least sqaured residuals
    residuals <- apply(meanDigitFrame[,-1],1,function(x){sum((digitLine-x)^2)})
    which.min(residuals)-1
}

forestDigits <- function(){
    
    if(!exists("digitTrain")) digitTrain <- read.csv("~/Kaggle/Digits/train.csv")
    if(!exists("digitTest")) digitTest <- read.csv("~/Kaggle/Digits/test.csv")
    set.seed(0)
    
    if(!exists("digitForest")){
        numTrain <- 5000
        numTrees <- 50
        
        rows <- sample(1:nrow(digitTrain), numTrain)
        labels <- as.factor(digitTrain[rows,1])
        train <- digitTrain[rows,-1]
        
        digitForest <- randomForest(train, labels, xtest=digitTest, ntree=numTrees,keep.forest = TRUE)
    }
    predictions <- data.frame(ImageId=1:nrow(digitTest), Label=levels(labels)[digitForest$test$predicted])
    
    write.csv(predictions, "~/Kaggle/Digits/RandomForest.csv",row.names = FALSE) 
}