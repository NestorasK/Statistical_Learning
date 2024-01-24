rm(list = ls())
library(ISLR)
data(Auto)

# (a)
pairs(Auto[, -9])

# (b)
names(Auto)
cor.matrix <- cor(Auto[, -9])

# (c)
lmFit <- lm(formula = mpg ~ . -name, data = Auto)
summary(lmFit)

# i 
# Yes, F statistic is high and the its respective pvalue is very small
# ii
# All predictor with statistical significant pvalue. Because the number of predictor is small we can rely on their p.values and no correction is needed.
# iii
# A positive relation to the response. The same relation is also present in the correlation matrix,
# cor(year, mpg) = 0.5805410 and the pairs plot 

# (d)
par(mfrow = c(2,2))
plot(lmFit)
# Non linearities are present in the data
# Observation 14 has high leverage
par(mfrow = c(1,1))
plot(hatvalues(lmFit), rstudent(lmFit), xlab = "Leverage", ylab = "Studentized Residuals")
# Outliers are present in the data. Observation with a Studentized Residual above 3 could be considered outliers.

# (e)
lmFit.inter1 <- lm(formula = mpg ~ . + cylinders:displacement -name, data = Auto)
summary(lmFit.inter1)
par(mfrow = c(2,2))
plot(lmFit.inter1)

lmFit.inter2 <- lm(formula = mpg ~ cylinders * displacement, data = Auto)
summary(lmFit.inter2)
plot(lmFit.inter2)

# (f)
# Log
X.log <- log(Auto[, -c(1,9)])
X.log$mpg <- Auto$mpg
lmFit.log <- lm(formula = mpg ~ ., data = X.log)
summary(lmFit.log)
par(mfrow = c(2,2))
plot(lmFit.log)

lmFit.log2 <- lm(formula = mpg ~ ., data = log(Auto[, -9]))
summary(lmFit.log2)
plot(lmFit.log2)

lmFit.logY <- lm(formula = log(mpg) ~ . -name, data = Auto)
summary(lmFit.logY)
plot(lmFit.logY)

# Square Root
lmFit.sqrt <- lm(formula = mpg ~ ., data = sqrt(Auto[,-c(9)]))
summary(lmFit.sqrt)
plot(lmFit.sqrt)

# Cubic
lmFit.cub <- lm(formula = mpg ~ poly(x = displacement, degree = 2) + 
                    poly(x = horsepower, degree = 2) + 
                    poly(x = weight, degree = 2) + 
                    poly(x = acceleration, degree = 2)  + 
                    year - name, 
                data = Auto)
summary(lmFit.cub)
plot(lmFit.cub)












