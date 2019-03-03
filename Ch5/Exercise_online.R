rm(list = ls())
require(boot)
load('Data/5.R.RData')

# Fit model
m1 <- lm(formula = y ~ X1 + X2, data = Xy)

summary(m1)$coefficients

# plotting the data
matplot(x = Xy, type = 'l')

# bootstrap
# calculate statistic
b1.coef <- function(data_i){
    m1 <- lm(formula = y ~ X1 + X2, data = data_i)
    return(m1$coefficients[2])
}

b1.coef.fn <- function(data_i, index){
    b1.coef(data_i = data_i[index, ])
}

# testing
b1.coef(data_i = Xy)
b1.coef.fn(data_i = Xy, index = 1:1000)
b1.coef.fn(data_i = Xy, index = sample(x = 1:1000, size = 1000, replace = TRUE))

# Running bootstrap
b1.boot <- boot(data = Xy, statistic = b1.coef.fn, R = 1000)
plot(b1.boot)

# block bootstrap
b1.block.boot <- tsboot(tseries = Xy, statistic = b1.coef.fn, R = 1000, 
                        sim = 'fixed', l = 100)









