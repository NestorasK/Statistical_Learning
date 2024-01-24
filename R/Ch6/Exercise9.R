### Exercise 9 ###
rm(list = ls())
set.seed(11)
library(ISLR)

# Load College dataset
attach(College)

# (a)
# Split dataset - 50 % for training
sum(is.na(College))
head(College)
train.index <- sample(x = 1:nrow(College), size = round(0.5 * nrow(College)))
test.index <- (-train.index)

# (b)
# Fitting a linear model using the training set
fit.linear <- lm(formula = Apps ~ ., data = College[train.index, ])
# Predictions
preds.linear <- predict(object = fit.linear, newdata = College[test.index, ])
# Mean Square error
mse <- mean(x = (preds.linear - College$Apps[test.index])^2)

# (c)
# Fitting a ridge regression
library(glmnet)
# Training set
trainSet <- model.matrix(object = Apps ~ ., data = College[train.index, ])[, -1]
# Test set
testSet <- model.matrix(object = Apps ~ ., data = College[test.index, ])[, -1]

# training - Using CV to find best lambda
# Set lambda grid
grid = 10 ^ seq(4, -2, length=100)
# Cross Validation
cv.ridge <- cv.glmnet(x = trainSet, y = College$Apps[train.index], alpha = 0, lambda = grid, 
                      thresh = 1e-12)
# test error - using minimum number or lambda
# Note that also the statistical equivalent value of lambda.min - lambda.1se could be used.
preds.ridge <- predict(object = cv.ridge, newx = testSet, s = cv.ridge$lambda.min)
# Mean Square Error
mse.ridge <- mean((preds.ridge - College$Apps[test.index])^2)

# Some plotting - Not required for the exercise
par(mfrow=c(1,2))
# Plot coefficients for the different values of lambda
plot(cv.ridge$glmnet.fit, xvar = "lambda")
# Plot Cross Validation errors
plot(cv.ridge)

# (d)
# Fitting a lasso regression
# training 
cv.lasso <- cv.glmnet(x = trainSet, y = College$Apps[train.index], alpha = 1, lambda = grid, 
                      thresh = 1e-12)
# test error - using minimum number or lambda
# Note that also the statistical equivalent value of lambda.min - lambda.1se could be used.
preds.lasso <- predict(object = cv.lasso, newx = testSet, s = cv.lasso$lambda.min)
# Mean Square Error
mse.lasso <- mean((preds.lasso - College$Apps[test.index])^2)
# Some plotting - Not required for the exercise
par(mfrow=c(1,2))
# Plot coefficients for the different values of lambda
plot(cv.lasso$glmnet.fit, xvar = "lambda")
# Plot Cross Validation errors
plot(cv.lasso)

# Coefficients
# Re-train using the full dataset
fit.lasso.full <- glmnet(x = model.matrix(object = Apps ~ ., data = College)[,-1], 
                         y = College$Apps, alpha = 1, lambda = grid, 
                         thresh = 1e-12)
coef.lasso <- predict(object = fit.lasso.full, s = cv.lasso$lambda.min, 
                      type = "coefficients")
cat("Lasso, number of non zero coefficients:", length(coef.lasso[coef.lasso != 0]))

# (e)
library(pls)
# Principle Components Regression 
# Fitting model on the training set using CV
fit.pcr <- pcr(Apps ~ ., data = College, subset = train.index, scale = TRUE,
               validation = "CV")
summary(fit.pcr)
# Generating a validation plot
par(mfrow=c(1,1))
validationplot(object = fit.pcr, val.type = "MSEP", type = "b")
preds.pcr <- predict(object = fit.pcr, newdata = testSet, ncomp = 5)
# Notes: 
# Selecting ncomp = 5 as the line is almost flattens out after that point.
# According to Chapter 10 you could select as few as three components as 
# at that point you can see the formation of the "elbow" shape.

# Mean Square Error
mse.pcr <- mean((preds.pcr - College$Apps[test.index])^2)
mse.pcr

# (f)
# Partial Least Square 
# Fitting model on the training set employing CV
fit.pls <- plsr(Apps ~ ., data = College, subset = train.index, scale = TRUE,
                validation = "CV")
validationplot(object = fit.pls, val.type = "MSEP", type = "b")
summary(fit.pls)
preds.pls <- predict(object = fit.pls, newdata = testSet, ncomp = 10)
# Mean Square Error
mse.pls <- mean((preds.pls - College$Apps[test.index])^2)

# (g)
barplot(height = c(mse, mse.ridge, mse.lasso, mse.pcr, mse.pls), 
        names.arg = c("Least Squares", "Ridge", "Lasso", "P.C.R", "PLS"), 
        ylab = "Mean Squared Error")







