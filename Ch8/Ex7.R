rm(list = ls())
library(randomForest)
library(MASS)
library(data.table)
library(ggplot2)
data(Boston)

set.seed(1)
train.Inds <- sample(x = nrow(Boston), size = nrow(Boston)*0.7)
mtry.range <- 1:(ncol(Boston) - 1) 
mtry_Errors <- rep(NA, times = length(mtry.range))
boston.test <- Boston[-train.Inds, "medv"]
mse_train <- data.frame(matrix(data = NA, nrow = 500, 
                          ncol = length(mtry.range)))
colnames(mse_train) <- paste("mtry", mtry.range, sep = ".")
mse_test <- mse_train

for(i in 1:length(mtry.range)){
    mtry_i <- mtry.range[i]
    rf.boston_i <- randomForest(medv~.,data=Boston,subset=train.Inds,
                                mtry=mtry_i,importance=TRUE)
    mse_train[, i] <- rf.boston_i$mse
    yhat.rf <- predict(object = rf.boston_i,
                       newdata = Boston[-train.Inds,], 
                       predict.all = TRUE)
    mse_test[1, i] <- 
        mean((yhat.rf$individual[, 1] - boston.test)^2)
    for(numtree_i in 2:ncol(yhat.rf$individual)){
        pred_numTree_i <- apply(X = yhat.rf$individual[, 1:numtree_i],
                                MARGIN = 1, FUN = mean)
        mse_test[numtree_i, i] <- 
            mean((pred_numTree_i - boston.test)^2)
    }

}
mse_test <- data.table(mse_test)
mse_test$numTrees <- 1:nrow(mse_test)
mse_test <- melt.data.table(data = mse_test, id.vars = "numTrees", 
                           variable.name = "mtry",
                           value.name = "mse")
ggplot(data = mse_test, 
       mapping = aes(x = numTrees, y = mse, colour = mtry)) + 
    geom_line()



