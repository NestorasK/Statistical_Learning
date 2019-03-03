rm(list = ls())
# (a)
set.seed(1)
x1 <- runif(100)
x2 <- 0.5*x1 + rnorm(100)/10
y <- 2 + 2*x1 + 0.3*x2 + rnorm(100)

# (b)
cor(x1, x2)
plot(x1, x2)

# (c)
lmFit <- lm(formula = y ~ x1 + x2)
summary(lmFit)
plot(hatvalues(model = lmFit), rstudent(model = lmFit))

# (d)
lmFit2 <- lm(formula = y ~ x1)
summary(lmFit2)
plot(hatvalues(model = lmFit2), rstudent(model = lmFit2))

# (e)
lmFit3 <- lm(formula = y ~ x2)
summary(lmFit3)
plot(hatvalues(model = lmFit3), rstudent(model = lmFit3))

# (f)
# Collinearity 

# (g)
x1 <- c(x1, 0.1)
x2<- c(x2, 0.8)
y <- c(y, 6)



