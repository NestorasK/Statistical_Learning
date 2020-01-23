rm(list = ls())
library(ISLR)
library(tree)
data("Carseats")
set.seed(12)

# a
trainInd <- sample(x = 1:nrow(Carseats), size = nrow(Carseats)*0.5)
Carseats.train <- Carseats[trainInd, ]
Carseats.test <- Carseats[-trainInd, ]

# b
tree.carseats <- tree(formula = Sales ~ ., data = Carseats.train)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats,pretty=0)
tree.pred <- predict(object = tree.carseats, newdata = Carseats.test)
# Mean Square Error
mean((tree.pred - Carseats.test$Sales)^2)

# c
cv.carseats <- cv.tree(object = tree.carseats)
plot(cv.carseats$k,cv.carseats$dev,type="b")
plot(cv.carseats$size,cv.carseats$dev,type="b")
which.min(cv.carseats$dev)
prune.carseats <- prune.tree(tree = tree.carseats, 
                             best = 14)
plot(prune.carseats)
text(prune.carseats,pretty=0)
prune.carseats.pred <- predict(object = prune.carseats, 
                               newdata = Carseats.test)
prune.pred <- predict(object = prune.carseats, newdata = Carseats.test)
# test MSE
mean((prune.pred - Carseats.test$Sales)^2)

# d
library(randomForest)
bag.carseats <- randomForest(formula = Sales ~ ., data = Carseats.train, 
                             mtry = 10, importance = TRUE)
bag.yhat <- predict(object = bag.carseats, newdata = Carseats.test)
plot(bag.carseats)
# test MSE
mean((bag.yhat - Carseats.test$Sales)^2)
# Importance
importance(bag.carseats)
varImpPlot(bag.carseats)

# e
rf.carseats <- randomForest(formula = Sales ~., data = Carseats.train, 
                            importance = TRUE)
plot(rf.carseats)
rf.yhat <- predict(object = rf.carseats, newdata = Carseats.test)
mean(x = (rf.yhat - Carseats.test$Sales)^2)
importance(rf.carseats)
varImpPlot(rf.carseats)
# Effect of mtry argument
# According to the book we can assume that train mse is close to test mse thus I will use the train mse to evaluate mtry variable's effect.
mtrys <- seq(from = 2, to = 10, by = 1)
errors_train <- matrix(data = NA, nrow = 500, ncol = length(mtrys))
mse_test <- rep(NA, times = length(mtrys))
for(i in 1:length(mtrys)){
    rf.carseats_i <- randomForest(formula = Sales ~., data = Carseats.train, 
                                  mtry = mtrys[i], importance = TRUE)
    errors_train[, i] <- rf.carseats_i$mse
    rf.yhat_i <- predict(object = rf.carseats_i, newdata = Carseats.test)
    mse_test[i] <- mean(x = (rf.yhat_i - Carseats.test$Sales)^2)
}
plot(x = mtrys, y = mse_test, type = "b")
points(x = mtrys[which.min(mse_test)], y = min(mse_test), 
       pch = 4, col = "red", cex = 2)
matplot(errors_train, type = "n", xlab = "numTrees", ylab = "MSE(train)")
matlines(errors_train, lty = 1, col = 1:ncol(errors_train))
legend(x = "topright", legend = paste("mtry", mtrys, sep = "="), 
       lty = 1, col = 1:ncol(errors_train))




