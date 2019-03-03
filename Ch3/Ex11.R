rm(list = ls())
set.seed(1)
x = rnorm(100)
y = 2 * x + rnorm(100)

# (a)
lmFit <- lm(formula = y ~ x + 0)
summary(lmFit)

# (b)
lmFit2 <- lm(formula = x ~ y + 0)
summary(lmFit2)

# (c)
# Different Coefficients
# Same t and F statistics

# (d)
# TODO


# (e)
# swaping y with x then t(x,y) = t(y,x)

# (f)
# Regress y on x
lmFit2 <- lm(formula = y ~ x)
summary(lmFit2)

# Regress x on y
lmFit3 <- lm(formula = x ~ y)
summary(lmFit3)


