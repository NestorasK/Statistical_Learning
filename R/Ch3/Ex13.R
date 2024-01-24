rm(list = ls())
set.seed(1)

# (a)
x <- rnorm(100)

# (b)
eps <- rnorm(n = 100, sd = 0.25)   

# (c)
y <- -1 + 0.5*x + rnorm(100)
# length of y
length(y)
b0 = -1
b1 = 0.5

# (d)
plot(x = x, y = y, pch = 20)

# (e)
lmFit <- lm(formula = y ~ x)
summary(lmFit)

# (f)
abline(lmFit, col = "blue", lty = 1)
legend(x = "bottomright", legend = "Least Square Line", lty = 1, 
       col = "blue")

# (g)
lmFit.poly <- lm(formula = y ~ poly(x = x, degree = 2))
summary(lmFit.poly)
# No evidence that the quadratic fit improves the fit
# There are evidence for the opposite
# The coefficient of x^2 term is not statistical significant, showing that this term is not needed for the fit
# The Rsquare has improved very sligtly and the RSE has reduced
# The coefficients of the quadratic fitted model are very far from the real model

# (h)
y <- -1 + 0.5*x + rnorm(100, sd = 0.1)
plot(x = x, y = y, pch = 20)
lmFit <- lm(formula = y ~ x)
summary(lmFit)
abline(lmFit, col = "blue", lty = 1)
legend(x = "bottomright", legend = "Least Square Line", lty = 1, 
       col = "blue")
lmFit.poly <- lm(formula = y ~ poly(x = x, degree = 2))
summary(lmFit.poly)
# No evidence that the quadratic fit improves the fit
# Even though the coefficient of x^2 term is statistical significant, showing that this term is needed for the fit, the model becomes more complex with minor improvement of the fit in terms of Rsquare and RSE improvements 

# (i)
y <- -1 + 0.5*x + rnorm(100, sd = 5)
plot(x = x, y = y, pch = 20)
lmFit <- lm(formula = y ~ x)
summary(lmFit)
abline(lmFit, col = "blue", lty = 1)
legend(x = "bottomright", legend = "Least Square Line", lty = 1, 
       col = "blue")
lmFit.poly <- lm(formula = y ~ poly(x = x, degree = 2))
summary(lmFit.poly)

# Evidence that the quadratic fit make the model worst than the linear fit.
# Rsquare and RSE show minor improvement in the quadratic fit in relation to the linear fit.
# The coefficient are less statistical significant in the quadratic fit than in the linear fit.
# The coefficient of the x^2 is not statistical significant.
# The Rsquare and RSE show improvement because we include on more variable to the fit.


















