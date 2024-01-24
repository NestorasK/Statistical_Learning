# Exercise 9, pg. 299
rm(list = ls())
library(MASS)
data("Boston")

# a
fit_i <- lm(formula = nox ~ poly(x = dis, degree = 3), data = Boston)
summary(fit_i)

dis.grid <- seq(from = min(Boston$dis), to = max(Boston$dis), by = 0.1)
nox_pred <- predict(object = fit_i, newdata = data.frame(dis = dis.grid),
                    se.fit = TRUE)
se.bands = cbind(nox_pred$fit + 2*nox_pred$se.fit, 
               nox_pred$fit - 2*nox_pred$se.fit)
plot(x = Boston$dis, y = Boston$nox, cex = .5, col="darkgrey", 
     xlab = "dis", ylab = "nox", ylim = c(0.2, 1), xlim = c(1,13), 
     main = "Cubic polynomial fit")
lines(x = dis.grid, y = nox_pred$fit, col = "red")
matlines(dis.grid, se.bands, lwd = 1, col = "blue", lty = 3)


# b
degrees <- c(1:10)
pdf(file = "Ch7/Ex9_PolynomialFit.pdf", width = 7, height = 9)
par(mfrow = c(4,3))
for(degree_i in degrees){
    fit_i <- lm(formula = nox ~ poly(x = dis, degree = degree_i),
                data = Boston)
    sumFit_i <- summary(fit_i)
    
    dis.grid <- seq(from = min(Boston$dis), to = max(Boston$dis), by = 0.1)
    nox_pred <- predict(object = fit_i, newdata = data.frame(dis = dis.grid),
                        se.fit = TRUE)
    se.bands=cbind(nox_pred$fit + 2*nox_pred$se.fit, 
                   nox_pred$fit - 2*nox_pred$se.fit)
    plot(x = Boston$dis, y = Boston$nox, cex = .5, col="darkgrey", 
         xlab = "dis", ylab = "nox", 
         ylim = c(floor(min(se.bands)), ceiling(max(se.bands))), 
         xlim = c(1,13), 
         main = paste("Degree:", degree_i, "| RSS:", 
                      round(x = sumFit_i$r.squared, digits = 3)))
    
    lines(x = dis.grid, y = nox_pred$fit, col = "red")
    matlines(dis.grid, se.bands, lwd = 1, col = "blue", lty = 3)
    
}
dev.off()


# c
library(boot)
set.seed(123)
degrees <- c(1:10)
cvMSE <- rep(x = NA, times = length(degrees))
for(degree_i in degrees){
    fit_i <- glm(formula = nox ~ poly(x = dis, degree = degree_i),
                 data = Boston)
    outCV <- cv.glm(data = Boston, glmfit = fit_i, K = 10)
    cvMSE[degree_i] <- outCV$delta[2]
}
plot(x = 1:length(cvMSE), y = cvMSE, type = "b", 
     xlab = "Degree of polynomial fit", ylab = "MSE (10 fold CV)")
points(x = which.min(cvMSE), y = min(cvMSE), pch = 19, col = "red")
legend(x = "topleft", legend = c("best degree", "not best degree"), 
       col = c("red", "black"), pch = c(19, 1))


# d
rm(list = ls())
library(splines)
# Fit 
fit_bs <- lm(formula = nox ~ bs(x = dis, df = 4), data = Boston)
# Knots
attr(x = bs(x = Boston$dis, df = 4), which = "knots")

# Plot
dis.grid <- seq(from = min(Boston$dis), to = max(Boston$dis), by = 0.1)
nox_pred <- predict(object = fit_bs, newdata = data.frame(dis = dis.grid),
                    se.fit = TRUE)
se.bands = cbind(nox_pred$fit + 2*nox_pred$se.fit, 
                 nox_pred$fit - 2*nox_pred$se.fit)
plot(x = Boston$dis, y = Boston$nox, cex = .5, col="darkgrey", 
     xlab = "dis", ylab = "nox", ylim = c(0.2, 1), xlim = c(1,13), 
     main = "Regression Spline fit")
lines(x = dis.grid, y = nox_pred$fit, col = "red")
matlines(dis.grid, se.bands, lwd = 1, col = "blue", lty = 3)



# e
dfs <- 3:10
dis.grid <- seq(from = min(Boston$dis), to = max(Boston$dis), by = 0.1)

pdf(file = "Ch7/Ex9_RegressionSpline.pdf")
par(mfrow = c(3,3))
for(df_i in dfs){
    
    fit_bs_i <- lm(formula = nox ~ bs(x = dis, df = df_i), data = Boston)
    sumfit_i <- summary(fit_bs_i)
    # Plot
    nox_pred <- predict(object = fit_bs_i, newdata = data.frame(dis = dis.grid),
                        se.fit = TRUE)
    se.bands = cbind(nox_pred$fit + 2*nox_pred$se.fit, 
                     nox_pred$fit - 2*nox_pred$se.fit)
    plot(x = Boston$dis, y = Boston$nox, cex = .5, col="darkgrey", 
         xlab = "dis", ylab = "nox", ylim = c(0.2, 1), xlim = c(1,13), 
         main = paste("Df", df_i, "| RSS", round(x = sumfit_i$r.squared, 
                                                 digits = 3)))
    lines(x = dis.grid, y = nox_pred$fit, col = "red")
    matlines(dis.grid, se.bands, lwd = 1, col = "blue", lty = 3)
    
}
dev.off()

# f
rm(list = ls())
set.seed(1)
data("Boston")
dfs <- 3:10
numFolds <- 10
config <- data.frame(dfs = rep(x = dfs, each = numFolds), 
                     fold = 1:numFolds, cvMSE = NA)
folds <- sample(x = 1:numFolds, size = nrow(Boston), replace = TRUE)
for(i in 1:nrow(config)){
    fit_bs_i <- lm(formula = nox ~ bs(x = dis, df = config$dfs[i]), 
                   data = Boston[folds != config$fold[i], ])
    pred_i <- predict(object = fit_bs_i, 
                      newdata = Boston[folds == config$fold[i], ])
    config$cvMSE[i] <- mean((pred_i - Boston$nox[folds == config$fold[i]])^2)
}
meanMSE <- tapply(X = config$cvMSE, INDEX = config$dfs, FUN = mean)
sdMSE <- tapply(X = config$cvMSE, INDEX = config$dfs, FUN = sd)

boxplot(formula = cvMSE ~ dfs, data = config, notch = TRUE)

boundariesMSEs <- cbind(meanMSE - sdMSE, meanMSE + sdMSE)
plot(x = dfs, y = meanMSE, type = "n", 
     xlab = "Degree of freedom", ylab = "MSE (10 fold CV)", 
     ylim = c(min(boundariesMSEs), max(boundariesMSEs)))
epsilon = 0.03
segments(x0 = dfs, y0 = boundariesMSEs[, 1],
         x1 = dfs, y1 = boundariesMSEs[, 2])
segments(x0 = dfs - epsilon, y0 = boundariesMSEs[,1], 
         x1 = dfs + epsilon, y1 = boundariesMSEs[, 1])
segments(x0 = dfs - epsilon, y0 = boundariesMSEs[,2], 
         x1 = dfs + epsilon, y1 = boundariesMSEs[, 2])
points(x = dfs, y = meanMSE, pch = 19)

# From above plots degrees of freedom equal to 3 is selected

















