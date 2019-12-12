rm(list = ls())
library(ISLR)
library(boot)
attach(Wage)

# a
## Cross Validation
set.seed(123)
nfolds <- 10
degrees <- 5
folds <- sample(x = c(1:nfolds), size = nrow(Wage), replace = TRUE)
cvError <- data.frame(mse = rep(x = NA, times = nfolds * degrees), 
                      folds = rep(1:nfolds, times = degrees), 
                      degrees = rep(1:degrees, each = nfolds))

for(i in 1:nrow(cvError)){
    fit.fold_i.degree_i <- lm(formula = wage ~ poly(x = age, degree = cvError$degrees[i]), 
                              data = Wage[folds != cvError$folds[i], c("age", "wage")])
    
    age2predict <- data.frame(age = Wage[folds == cvError$folds[i], c("age")])
    
    wage_pred_i <- predict(object = fit.fold_i.degree_i, newdata = age2predict)
    
    cvError$mse[i] <- mean((Wage[folds == cvError$folds[i], "wage"] - wage_pred_i)^2)

}
meanMSE <- tapply(X = cvError$mse, INDEX = cvError$degrees, FUN = mean)

boxplot(formula = mse ~ degrees, data = cvError, main = "10 fold Cross validation results")
plot(x = meanMSE, xlab = "Degrees", ylab = "Mean MSE", main = "10 fold Cross validation results", 
     type = "p")
points(x = which.min(meanMSE), y = meanMSE[which.min(meanMSE)], 
       pch = 4, col = "red", cex = 2, lwd = 2)

# Anova results
fit_d1 <- lm(formula = wage ~ poly(x = age, degree = 1), 
             data = Wage)
fit_d2 <- lm(formula = wage ~ poly(x = age, degree = 2), 
             data = Wage)
fit_d3 <- lm(formula = wage ~ poly(x = age, degree = 3), 
             data = Wage)
fit_d4 <- lm(formula = wage ~ poly(x = age, degree = 4), 
             data = Wage)
fit_d5 <- lm(formula = wage ~ poly(x = age, degree = 5), 
             data = Wage)
anova(fit_d1, fit_d2, fit_d3, fit_d4, fit_d5)


# Plot
age.grid <- seq(from = range(age)[1], range(age)[2])
preds <- predict(object = fit_d4, newdata = list(age = age.grid), 
                 se.fit = TRUE)
conf.int <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)
plot(x = age, y = wage, col = "gray", main = "Polynomial fit, 4th degree")
lines(x = age.grid, y = preds$fit, lwd = 2, col ="blue") 
matlines(age.grid, conf.int ,lwd=1, col="blue",lty =3)


# b 
## Cross Validation
rm(list = ls())
library(ISLR)
data(Wage)
set.seed(123)

numSteps <- 10
cvError <- data.frame(mse = rep(x = NA, times = numSteps), 
                      steps = rep(2:(numSteps+1)))
for(i in 1:nrow(cvError)){
    Wage$age.cut <- cut(Wage$age, cvError$steps[i])
    fit.stepFunction <- glm(formula = wage ~ age.cut, data = Wage)
    cvError$mse[i] <- cv.glm(data = Wage, glmfit = fit.stepFunction, K = 10)$delta[1]
}
plot(x = cvError$steps, y = cvError$mse, main = "10 fold CV results", xlab = "Number of cuts", 
     ylab = "MSE")
points(x = cvError$steps[which.min(cvError$mse)], y = cvError$mse[which.min(cvError$mse)], 
       pch = 4, col = "red", cex = 2, lwd = 2)

# Plot
fit.stepFunction <- glm(formula = wage ~ cut(age, breaks = 11), data = Wage)

age.grid <- seq(from = range(age)[1], range(age)[2])

preds <- predict(object = fit.stepFunction, newdata = data.frame(age = age.grid), 
                 se.fit = TRUE)

conf.int <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)
plot(x = age, y = wage, col = "gray", main = "Step function fit, 10 cuts")
lines(x = age.grid, y = preds$fit, lwd = 2, col ="blue") 
matlines(age.grid, conf.int ,lwd=1, col="blue",lty =3)














