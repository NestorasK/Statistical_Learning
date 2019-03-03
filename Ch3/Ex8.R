rm(list = ls())
library(ISLR)
data(Auto)

# (a)
fit.lm <- lm(formula = mpg ~ horsepower, data = Auto)
summary(fit.lm)
# i
# Yes the pvalue is significant

# ii
# Very strong 
# The std error is small 

# iii
# Negative 

# iv
predict(object = fit.lm, newdata = data.frame(horsepower = 98), interval = "confidence")
predict(object = fit.lm, newdata = data.frame(horsepower = 98), interval = "prediction")

# (b)
plot(x = Auto$horsepower, y = Auto$mpg, xlab = "horsepower", ylab = "mpg", pch = 20)
abline(fit.lm, col = "red")

# (c)
par(mfrow = c(2,2))
plot(fit.lm)
par(mfrow = c(1,2))
# Non linearities in the data
plot(predict(fit.lm), residuals(fit.lm), xlab = "Fitted Values", ylab = "Residuals")

# High leverage points - outliers 
plot(hatvalues(fit.lm), rstudent(fit.lm), xlab = "Leverage", ylab = "Studentized Residuals")





