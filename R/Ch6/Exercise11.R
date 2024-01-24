# Exercise 11 # 
rm(list = ls())
library(MASS)
data(Boston)
set.seed(123)

# (a)
# training set
train.index <- sample(x = 1:nrow(Boston), size = 0.7 * nrow(Boston))
trainSet <- Boston[train.index, ]
testSet <- Boston[-train.index, ]

# Best Subset Selection #
library(leaps)
# Predictions functions
predict.regsubsets=function(object,newdata,id,...){
    form=as.formula(object$call[[2]])
    mat=model.matrix(form,newdata)
    coefi=coef(object,id=id)
    xvars=names(coefi)
    mat[,xvars]%*%coefi
}
# Ten fold Cross Validation
nfolds <- 10
folds <- sample(x = c(1:nfolds), size = nrow(trainSet), replace = TRUE)
# Initialize table
CV_MSEs <- matrix(data = NA, nrow = nfolds, ncol = ncol(Boston) - 1)
for(i in 1:nfolds){
    # Training Set
    trainSet_i <- trainSet[folds != i, ]
    # Test set
    testSet_i <- trainSet[folds == i, ]
    # Fitting model
    fit.cv <- regsubsets(x = crim ~ ., data = trainSet_i, nvmax = ncol(trainSet_i))
    for(j in 1:(ncol(trainSet_i)-1)){
        # Predictions
        pred_j <- predict.regsubsets(object = fit.cv, newdata = testSet_i, id = j)
        # Calculate Mean Square Error per id
        CV_MSEs[i, j] <- mean((pred_j - testSet_i$crim)^2)
    }
}
# Mean MSE per model size
mean_mses <- apply(X = CV_MSEs, MARGIN = 2, FUN = mean)
# Plots
par(mfrow = c(1,2))
boxplot(CV_MSEs, xlab = "Model Size", ylab = "CV Mean Square Error", main = "Best Subset Selection")
plot(x = mean_mses, type = "b")
points(x = which.min(mean_mses), min(mean_mses), col = "red", pch = 19, xlab = "Model Size", 
       ylab = "Mean CV MSE", main = "Best Subset Selection")

# test MSE
# fit model in the full TRAINING dataset for the best number of features found from CV
fit.train <- regsubsets(x = crim ~ ., data = trainSet, nvmax = 2)
coef(object = fit.train, id = 2)
# Predictions on test set
pred.test <- predict.regsubsets(object = fit.train, newdata = testSet, id = 2)
# Final Mean Square Error
mse.test <- mean((pred.test - testSet$crim)^2)

# Ridge Regression #
library(glmnet)
# Generate my grid
grid <- 10^seq(10,-2,length=100)
# Fit ridge regression 
# Ten fold cross validation is used to identify the best lambda
fit.ridge <- cv.glmnet(x = model.matrix(object = crim ~ ., data = trainSet)[, -1], 
                       y = trainSet$crim, lambda = grid, alpha = 0)
# Plot the model
par(mfrow = c(1,2))
plot(fit.ridge)
plot(fit.ridge$glmnet.fit, xvar = "lambda")
# Evaluate test set
pred.ridge <- predict(object = fit.ridge, s = fit.ridge$lambda.min,
                      newx = model.matrix(object = crim ~ ., data = testSet)[, -1])
mse.ridge <- mean((pred.ridge - testSet$crim)^2)

# Lasso # 
# Fit lasso
# Ten fold cross validation is used to identify the best lambda
fit.lasso <- cv.glmnet(x = model.matrix(object = crim ~ ., data = trainSet)[, -1], 
                       y = trainSet$crim, lambda = grid, alpha = 1)
# Plot the model
par(mfrow = c(1,2))
plot(fit.lasso)
plot(fit.lasso$glmnet.fit, xvar = "lambda")
# Evaluate test set
pred.lasso <- predict(object = fit.lasso, s = fit.lasso$lambda.min,
                      newx = model.matrix(object = crim ~ ., data = testSet)[, -1])
mse.lasso <- mean((pred.lasso - testSet$crim)^2)

# PCR
library(pls)
# Principle Components Regression 
# Fitting model on the training set using CV
fit.pcr <- pcr(crim ~ ., data = trainSet, scale = TRUE, validation = "CV")
summary(fit.pcr)
# Generating a validation plot
par(mfrow=c(1,1))
validationplot(object = fit.pcr, val.type = "MSEP", type = "b")
preds.pcr <- predict(object = fit.pcr, newdata = testSet, ncomp = 8)
# Notes: 
# Selecting ncomp = 8 as the line is almost flattens out after that point.
# According to Chapter 10 you could select as few as three components as 
# at that point you can see the formation of the "elbow" shape.

# Mean Square Error
mse.pcr <- mean((preds.pcr - testSet$crim)^2)
mse.pcr

# Compare methods using a barplot
barplot(height = c(mse.test, mse.ridge, mse.lasso, mse.pcr), 
        names.arg = c("BestSubsetSel", "RidgeRegression", "Lasso", "PCR"), 
        ylab = "Mean Square Error")

# From the barplot lasso in the best model
# Retrain lasso on the full dataset
fit.lasso.full <- glmnet(x = model.matrix(object = crim ~ ., data = Boston)[, -1],
                         y = Boston$crim, lambda = grid, alpha = 1)
coef.lasso <- predict(object = fit.lasso.full, type = "coef", s = fit.lasso$lambda.min)
plot(fit.lasso.full, xvar = "lambda")












