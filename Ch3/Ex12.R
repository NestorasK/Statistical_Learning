rm(list = ls())
# (a)
# When the Sum of the squares of x are equal to the sum of the square of y
# (b)
X <- rnorm(n = 100)
Y <- rnorm(n = 100)
summary(object = lm(formula = Y ~ X + 0 ))
summary(object = lm(formula = X ~ Y + 0 ))

# (c)
Y <- sample(X, 100)
sum(Y^2)
sum(X^2)
summary(object = lm(formula = Y ~ X + 0 ))
summary(object = lm(formula = X ~ Y + 0 ))





