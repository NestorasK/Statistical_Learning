# Exercise svm Statistical learning
rm(list = ls())
require(e1071)
require(foreach)
require(doMC)

registerDoMC(cores = detectCores())
errorRates <- foreach(1:1000, .combine = c) %dopar%{
    
    # Generate data
    numSamples0 <- 1000
    numFeatures0 <- 10
    
    # Class 0 data
    data0 <- matrix(rnorm(n = numFeatures0 * numSamples0, mean = 0), 
                    nrow = numSamples0, ncol = numFeatures0)
    
    # Class 1 data
    data1.0 <- matrix(rnorm(n = numFeatures0/2 * numSamples0, mean = 0), 
                      nrow = numSamples0, ncol = numFeatures0/2)
    data1.1 <- matrix(rnorm(n = numFeatures0/2 * numSamples0, mean = 1), 
                      nrow = numSamples0, ncol = numFeatures0/2)
    data1 <- cbind(data1.1, data1.0)
    
    # Training data
    trainingData <- data.frame(rbind(data0[c(1:50), ], data1[c(1:50),]))
    classTrain <- rep(c(0, 1), each = 50)
    # Test data
    testData <- data.frame(rbind(data0[-c(1:50), ], data1[-c(1:50),]))
    classTest <- rep(c(0, 1), each = 950)
    
    # Fit SVM
    # svmfit <- svm(x = trainingData, y = classTrain, kernel = "linear")
    
    # Fit logistic regression
    train.df <- data.frame(trainingData, classTrain)
    glmFit <- glm(formula = classTrain ~ ., data = train.df, 
                  family = binomial(link = "logit"))
    
    # Predict test
    predTest <- predict(object = glmFit, newdata = testData, type = 'response')
    # Get class predictions
    predTest <- ifelse(predTest > 0.5,1,0)
    
    # Error rate 
    results <- table(classTest, predTest)
    errorRate <- (1900 - sum(diag(results))) / 1900
    
    return(errorRate)
    
}



mean(errorRates)
