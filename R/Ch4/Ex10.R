rm(list = ls())
library(ISLR)
data("Weekly")

# a)
dim(Weekly)
summary(Weekly)
pairs(x = Weekly)
cor(Weekly[,-9])

# b)
glm.fit <- glm(formula = Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
               family = binomial, data = Weekly)
summary(glm.fit)

# c)
glm.probs <- predict(glm.fit, type="response")
glm.probs[1:5]
contrasts(Weekly$Direction)
glm.pred <- rep("Down", nrow(Weekly))
glm.pred[glm.probs > 0.5] <- "Up"
table(glm.pred, Weekly$Direction)
mean(glm.pred == Weekly$Direction)

# d)
train.ind <- Weekly$Year < 2009
glm.fit2 <- glm(formula = Direction ~ Lag2, family = binomial, data = Weekly, 
                subset = train.ind)
glm.probs2 <- predict(object = glm.fit2, newdata = Weekly[!train.ind, ], 
                      type = "response")
glm.pred2 <- ifelse(test = glm.probs2 > 0.5, yes = "Up", "Down")
table(glm.pred2, Weekly$Direction[!train.ind])
mean(glm.pred2 == Weekly$Direction[!train.ind])

# e)
library(MASS)
lda.fit <- lda(formula = Direction ~ Lag2, data = Weekly, subset = train.ind)
lda.fit
lda.pred <- predict(object = lda.fit, newdata = Weekly[!train.ind, ])
table(lda.Pred = lda.pred$class, true = Weekly$Direction[!train.ind])
mean(lda.pred$class == Weekly$Direction[!train.ind])

# f)
qda.fit <- qda(formula = Direction ~ Lag2, data = Weekly, subset = train.ind)
qda.pred <- predict(object = qda.fit, newdata = Weekly[!train.ind, ])
table(qda.pred = qda.pred$class, true = Weekly$Direction[!train.ind])
mean(qda.pred$class == Weekly$Direction[!train.ind])

# g)
library(class)
ki = 1
set.seed(1)
knn.pred <- knn(train = data.frame(Weekly$Lag2[train.ind]), 
                test = data.frame(Weekly$Lag2[!train.ind]), 
                cl = Weekly$Direction[train.ind], k = ki)
table(knn.pred, Weekly$Direction[!train.ind])
cat("k =", ki, ", acc =", mean(knn.pred == Weekly$Direction[!train.ind]), "\n")

# h
# lda











