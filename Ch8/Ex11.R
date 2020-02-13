rm(list = ls())
library(gbm)
library(ISLR)
library(class)
data("Caravan")
set.seed(123)
# a
Caravan.train <- Caravan[1:1000, ]
Caravan.test <- Caravan[-c(1:1000), ]

# b
Caravan.train$Purchase <- ifelse(test = Caravan.train$Purchase == "Yes", 
                                 yes = 1, no = 0)
boost.caravan <- gbm(formula = Purchase ~., data = Caravan.train, 
                     distribution = "bernoulli", n.trees = 10000, 
                     shrinkage = 0.01)
summary(boost.caravan)

# c
Caravan.test$Purchase <- ifelse(test = Caravan.test$Purchase == "Yes", 
                                 yes = 1, no = 0)
prob_hat <- predict(object = boost.caravan, newdata = Caravan.test, 
                    n.trees = 1000, type = "response")
yhat <- factor(x = ifelse(test = prob_hat > 0.2, yes = 1, no = 0))
confmat <- table(yhat, Caravan.test$Purchase)
confmat
# What fraction of the people predicted to make a purchase do in fact make one?
confmat[2,2] / sum(confmat[2,])


# KNN
Caravan.scale <- scale(x = Caravan[, -86])
yhat <- knn(train = Caravan.scale[1:1000, -86], 
            cl = factor(Caravan[1:1000, "Purchase"]),
            test = Caravan.scale[-c(1:1000),],
            k = 5)
confmat <- table(yhat, Caravan.test$Purchase)
confmat
# What fraction of the people predicted to make a purchase do in fact make one?
confmat[2,2] / sum(confmat[2,])


# Logistic regression
glm.caravan <- glm(formula = Purchase ~ .,family = binomial, 
                   data = Caravan.train)
prob_hat <- predict(object = glm.caravan, newdata = Caravan.test, 
                    type = "response")
yhat <- factor(x = ifelse(test = prob_hat > 0.2, yes = 1, no = 0))
confmat <- table(yhat, Caravan.test$Purchase)
confmat[2,2] / sum(confmat[2,])

