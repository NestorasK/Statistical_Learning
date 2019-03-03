### Exercise 8 ###
rm(list = ls())
set.seed(1)
library(leaps)
# (a)
X <- rnorm(n = 100)
e <- rnorm(n = 100)
# (b)
betas <- rnorm(n = 4, mean = 0, sd = 2)
beta0 <- betas[1]
beta1 <- betas[2]
beta2 <- betas[3]
beta3 <- betas[4]

Y <- beta0 + beta1*X + beta2*X^2 + beta3*X^3 + e

# (c)
features <- data.frame(poly(X, 10, raw = TRUE))
features$Y <- Y
# Best Subset Selection
fit.best <- regsubsets(x = Y ~ ., data = features, nvmax = 10)
fit.best.summary <- summary(fit.best)
# Plotting
par(mfrow = c(2,2))
# Cp 
plot(fit.best.summary$cp, type = "b", xlab = "Subset Size", ylab = "Cp", 
     main = "Best Subset Selection")
points(x = which.min(fit.best.summary$cp), y = fit.best.summary$cp[which.min(fit.best.summary$cp)], 
       pch = 19, col = "red")
# BIC
plot(fit.best.summary$bic, type = "b", xlab = "Subset Size", ylab = "BIC", 
     main = "Best Subset Selection")
points(x = which.min(fit.best.summary$bic), y = fit.best.summary$bic[which.min(fit.best.summary$bic)], 
       pch = 19, col = "red")
# adjr2
plot(fit.best.summary$adjr2, type = "b", xlab = "Subset Size", ylab = "Adj R^2", 
     main = "Best Subset Selection")
points(x = which.max(fit.best.summary$adjr2), y = fit.best.summary$adjr2[which.max(fit.best.summary$adjr2)], 
       pch = 19, col = "red")

# coefficients
coef(object = fit.best, 4)
coef(object = fit.best, 3)

# (d)
# Forward selection
fit.fw <- regsubsets(x = Y ~ ., data = features, nvmax = 10, method = "forward")
fit.fw.summary <- summary(fit.fw)
par(mfrow = c(2,2))
# Cp 
plot(fit.fw.summary$cp, type = "b", xlab = "Subset Size", ylab = "Cp", main = "Forward Selection")
points(x = which.min(fit.fw.summary$cp), y = fit.fw.summary$cp[which.min(fit.fw.summary$cp)], 
       pch = 19, col = "red")
# BIC
plot(fit.fw.summary$bic, type = "b", xlab = "Subset Size", ylab = "BIC", main = "Forward Selection")
points(x = which.min(fit.fw.summary$bic), y = fit.fw.summary$bic[which.min(fit.fw.summary$bic)], 
       pch = 19, col = "red")
# adjr2
plot(fit.fw.summary$adjr2, type = "b", xlab = "Subset Size", ylab = "Adj R^2", 
     main = "Forward Selection")
points(x = which.max(fit.fw.summary$adjr2), y = fit.fw.summary$adjr2[which.max(fit.fw.summary$adjr2)], 
       pch = 19, col = "red")
# coefficients
coef(object = fit.fw, 3)
coef(object = fit.fw, 4)


# Backward selection
fit.bw <- regsubsets(x = Y ~ ., data = features, nvmax = 10, method = "backward")
fit.bw.summary <- summary(fit.bw)
par(mfrow = c(2,2))
# Cp 
plot(fit.bw.summary$cp, type = "b", xlab = "Subset Size", ylab = "Cp", main = "backward Selection")
points(x = which.min(fit.bw.summary$cp), y = fit.bw.summary$cp[which.min(fit.bw.summary$cp)], 
       pch = 19, col = "red")
# BIC
plot(fit.bw.summary$bic, type = "b", xlab = "Subset Size", ylab = "BIC", main = "backward Selection")
points(x = which.min(fit.bw.summary$bic), y = fit.bw.summary$bic[which.min(fit.bw.summary$bic)], 
       pch = 19, col = "red")
# adjr2
plot(fit.bw.summary$adjr2, type = "b", xlab = "Subset Size", ylab = "Adj R^2", 
     main = "backward Selection")
points(x = which.max(fit.bw.summary$adjr2), y = fit.bw.summary$adjr2[which.max(fit.bw.summary$adjr2)], 
       pch = 19, col = "red")
# coefficients
coef(object = fit.bw, 3)
coef(object = fit.bw, 4)

# (e)
# lasso
library(glmnet)
predictors <- model.matrix(object = Y ~ ., data = features)[, -1]
train.index <- sample(x = 1:nrow(predictors), size = 70, replace = FALSE)
test.index <- (-train.index)
response <- features$Y
grid <- 10^seq(10,-2,length=100)
# training 
lasso.mod <- glmnet(x = predictors[train.index, ], y = response[train.index], 
                    alpha = 1, lambda = grid)
par(mfrow = c(1,1))
plot(lasso.mod, xvar = "lambda")
# Lambda selection - Cross validation
cv.lasso <- cv.glmnet(x = predictors[train.index, ], y = response[train.index], 
                      alpha = 1, lambda = grid)
plot(cv.lasso)
bestlam <- cv.lasso$lambda.min
# test error
lasso.pred = predict(lasso.mod, s = bestlam, newx = predictors[test.index, ])
mean((lasso.pred - response[test.index])^2)
# Final model
final.model <- glmnet(x = predictors, y = response, alpha = 1, lambda = grid)
lasso.coef <- predict(final.model, type = "coefficients", s = bestlam)

# (f)
features$Y <- 5 + 7*X^7 + e

# best subset selection
fit.best <- regsubsets(x = Y ~ ., data = features, nvmax = 10)
fit.best.summary <- summary(fit.best)
# Plotting
par(mfrow = c(2,2))
# Cp 
plot(fit.best.summary$cp, type = "b", xlab = "Subset Size", ylab = "Cp", main = "Best Subset Selection")
points(x = which.min(fit.best.summary$cp), y = fit.best.summary$cp[which.min(fit.best.summary$cp)], 
       pch = 19, col = "red")
# BIC
plot(fit.best.summary$bic, type = "b", xlab = "Subset Size", ylab = "BIC", main = "Best Subset Selection")
points(x = which.min(fit.best.summary$bic), y = fit.best.summary$bic[which.min(fit.best.summary$bic)], 
       pch = 19, col = "red")
# adjr2
plot(fit.best.summary$adjr2, type = "b", xlab = "Subset Size", ylab = "Adj R^2", 
     main = "Best Subset Selection")
points(x = which.max(fit.best.summary$adjr2), y = fit.best.summary$adjr2[which.max(fit.best.summary$adjr2)], 
       pch = 19, col = "red")
# coefficients
coef(object = fit.best, 1) # BIC
coef(object = fit.best, 2) # Cp
coef(object = fit.best, 4) # adjR^2

# lasso
f.pred <- model.matrix(object = Y ~ ., data = features)[, -1]
resp <- features$Y
# training
fit.lasso <- glmnet(x = f.pred[train.index, ], y = resp[train.index],
                    lambda = grid, alpha = 1)
par(mfrow = c(1,1))
plot(fit.lasso, xvar = "lambda")
cv.lasso <- cv.glmnet(x = f.pred[train.index, ], y = resp[train.index], 
                      lambda = grid, alpha = 1)
bestlam <- cv.lasso$lambda.min
plot(cv.lasso)
lasso.pred <- predict(object = fit.lasso, s = bestlam, 
                      newx = f.pred[-train.index, ])
mean((resp[-train.index] - lasso.pred)^2)
# final model
lasso.full <- glmnet(x = f.pred, y = resp, lambda = grid, alpha = 1)
lasso.coef <- predict(object = lasso.full, type = "coefficients", s = bestlam)
lasso.coef

























