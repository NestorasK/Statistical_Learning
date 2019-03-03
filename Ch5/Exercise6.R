rm(list = ls())
library(ISLR)
library(boot)
# library(doMC)
set.seed(123)
data("Default")

# a
fit.glm <- glm(formula = default ~ income + balance, family = binomial, data = Default)
summary(fit.glm)$coef


# b
boot.fn <- function(datai, indexi){
    return(coef(glm(formula = default ~ income + balance, family = binomial,
                    data = datai, subset = indexi))[-1])
}

# c
system.time({
    results.boot <- boot(data = Default, statistic = boot.fn, R = 1000, 
                         parallel = "multicore", ncpus = 4)
})



    