rm(list = ls())
library(MASS)
library(boot)

data("Boston")
summary(Boston)

# a
mymean <- mean(Boston$medv)

# b
mymean.ste <- sd(Boston$medv) / sqrt(length(Boston$medv))

# c
boot.fn <- function(datai, indexi){
   mean(datai$medv[indexi]) 
}
results.boot <- boot(data = Boston, statistic = boot.fn, R = 1000)
results.boot

# d
boot.ConfInt <- c(mymean - 2*sd(results.boot$t), mymean + 2*sd(results.boot$t)) 
t.test(Boston$medv)

# e
my.median <- median(Boston$medv)

# f
median.fn <- function(datai, indexi){
    median(datai$medv[indexi]) 
}
median.boot <- boot(data = Boston, statistic = median.fn, R = 1000)
median.boot

# g
perc.10 <- quantile(x = Boston$medv, probs = 0.1)

# h
perc10.fn <- function(datai, indexi){
    quantile(x = datai$medv[indexi], probs = 0.1) 
}
boot.perc10 <- boot(data = Boston, statistic = perc10.fn, R = 1000)
boot.perc10


















