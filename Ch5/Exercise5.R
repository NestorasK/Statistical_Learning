rm(list = ls())
library(ISLR)

set.seed(123)
data("Default")

# a
fit.glm <- glm(formula = default ~ income + balance, data = Default, 
               family = "binomial")
# b
val.errors <- rep(NA, 3)
for(i in 1:3){
    train <- sample(x = 1:nrow(Default), size = nrow(Default)*0.5)
    fit.glm <- glm(formula = default ~ income + balance, data = Default, 
                   family = "binomial", subset = train)
    contrasts(Default$default)
    
    probs_i <- predict(object = fit.glm, newdata = Default[-train,], 
                       type = "response")
    preds_i <- rep(NA, length(probs_i))
    preds_i[probs_i > 0.5] <- "Yes"
    preds_i[probs_i <= 0.5] <- "No"
    preds_i <- as.factor(preds_i)
    
    val.errors[i] <- mean(Default$default[-train] != preds_i)
    
}


# c

# d
val.errors2 <- rep(NA, 3)
for(i in 1:3){
    train <- sample(x = 1:nrow(Default), size = nrow(Default)*0.5)
    fit.glm <- glm(formula = default ~ income + balance + student, data = Default, 
                   family = "binomial", subset = train)
    contrasts(Default$default)
    
    probs_i <- predict(object = fit.glm, newdata = Default[-train,], 
                       type = "response")
    preds_i <- rep(NA, length(probs_i))
    preds_i[probs_i > 0.5] <- "Yes"
    preds_i[probs_i <= 0.5] <- "No"
    preds_i <- as.factor(preds_i)
    
    val.errors2[i] <- mean(Default$default[-train] != preds_i)
    
}

