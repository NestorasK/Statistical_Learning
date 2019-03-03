rm(list = ls())
library(boot)

# (a)
set.seed(1)
x <- rnorm(100)
y <- x-2*x^2+rnorm(100)
# n = 100, p = 2

# b
plot(x = x, y = y)

# (c)
set.seed(1)
mydata <- data.frame(x, y)

cv.errors <- rep(x = NA, 4)
for(i in 1:4){
    fit.glm <- glm(formula = y ~ poly(x = x, degree = i), data = mydata)
    print(summary(fit.glm))
    fit.glm.LOOCV <- cv.glm(data = mydata, glmfit = fit.glm)
    cv.errors[i] <- fit.glm.LOOCV$delta[1]
    
}

plot(x = c(1:4), y = cv.errors, type = "b",
     main = "LOOCV", ylab = "MSE", xlab = "Degree of Polynomial",
     xlim = c(1, 4), 
     ylim = c(0, 10))




