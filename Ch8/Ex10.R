rm(list = ls())
library(gbm)
library(ISLR)
data(Hitters)
set.seed(123)

# a
which(x = is.na(Hitters), arr.ind = TRUE)
# NAs are only in the salary column, thus we can use na.omit to remove NAs
Hitters <- na.omit(Hitters)
inds <- which(x = colnames(Hitters) %in% c("League", "Division", "NewLeague"))
Hitters.nums <- Hitters[, -inds]
Hitters.nums <- data.frame(apply(X = Hitters.nums, MARGIN = 2, 
                                     FUN = as.numeric))
Hitters <- cbind(Hitters.nums, Hitters[, inds])
Hitters$Salary <- log(Hitters$Salary)

# b
Hitters.train <- Hitters[1:200, ]
Hitters.test <- Hitters[-c(1:200), ]

# c
lambdas <- seq(from = 0.001, to = 0.1, length.out = 10)
MSEs_train <- rep(x = NA, times = length(lambdas))
MSEs_test <- rep(x = NA, times = length(lambdas))
MSEs_cv <- rep(x = NA, times = length(lambdas))

for(i in 1:length(lambdas)){
    boost.Hitters_i <- gbm(formula = Salary ~., 
                           distribution = "gaussian", 
                           data = Hitters.train, 
                           n.trees = 1000, shrinkage = lambdas[i], 
                           cv.folds = 5)
    MSEs_train[i] <- boost.Hitters_i$train.error[1000]
    MSEs_cv[i] <- boost.Hitters_i$cv.error[1000]
    yhat_salary_test <- predict(object = boost.Hitters_i, 
                                newdata = Hitters.test, n.trees = 1000)
    MSEs_test[i] <- mean((yhat_salary_test - Hitters.test$Salary)^2)
}
plot(x = lambdas, y = MSEs_train, type = "b", col = "blue", ylab = "MSEs", 
     xlab = "shrinkage", ylim = c(0, 0.5))
lines(x = lambdas, y = MSEs_cv, type = "b", col = "green")
# d
lines(x = lambdas, y = MSEs_test, type = "b", col = "red")
legend(x = "topright", legend = c("MSE_train", "MSE_cv", "MSE_test"), 
       lty = 1, col = c("blue", "green", "red"))

# Train a model using all training data based on cv error
boost.Hitters <- gbm(formula = Salary ~., 
                     distribution = "gaussian", 
                     data = Hitters.train, 
                     n.trees = 1000, shrinkage = lambdas[which.min(MSEs_cv)])

# e
# Multiple Linear Regression - Chapter 3
lm.Hitters <- lm(formula = Salary ~., data = Hitters.train)
yhat_salary_test_lm <- predict(object = lm.Hitters, newdata = Hitters.test)
MSEs_test_lm <- mean((yhat_salary_test_lm - Hitters.test$Salary)^2)
MSEs_test_lm
# LASSO - Chapter 6
library(glmnet)
x = model.matrix(Salary~.,Hitters)[,-1]
y = Hitters$Salary
x.train = x[1:200, ]
x.test = x[-c(1:200), ]
y.train = y[1:200]
y.test = y[-c(1:200)]
lasso.Hitters = cv.glmnet(x = x.train, y = y.train, alpha = 1)
yhat_salary_test_lasso = predict(object = lasso.Hitters, newx = x.test, 
                                 s = lasso.Hitters$lambda.min)
MSEs_test_lasso = mean((yhat_salary_test_lasso - y.test)^2)
MSEs_test_lasso
# Compare methods
MSEs = c(MSEs_test[which.min(MSEs_cv)], MSEs_test_lasso, MSEs_test_lm)
names(MSEs) = c("gbm", "lasso", "lm")
barplot(height = MSEs, las = 1, ylab = "test MSE")

# f
summary(boost.Hitters, las = 1)

# g
library(randomForest)
bag.Hitters = randomForest(formula = Salary ~., data = Hitters.train,
                           mtry = 19)
plot(bag.Hitters)
yhat_salary_test_bag = predict(object = bag.Hitters, newdata = Hitters.test)
MSE_bag = mean((yhat_salary_test_bag - Hitters.test$Salary)^2)
MSE_bag
MSEs = c(MSE_bag, MSEs)
names(MSEs)[1] <- "bag"
barplot(height = MSEs, las = 1, ylab = "test MSE", xlab = "Method")
