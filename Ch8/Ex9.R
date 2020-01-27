rm(list = ls())
library(ISLR)
data("OJ")
set.seed(123)

# a
inds.train <- sample(x = 1:nrow(OJ), size = 800)
OJ.train <- OJ[inds.train, ]
OJ.test <- OJ[-inds.train, ]

# b
library(tree)
tree.OJ.train <- tree(formula = Purchase ~ ., data = OJ.train)
summary(tree.OJ.train)

# c
tree.OJ.train

# d
plot(tree.OJ.train)
text(x = tree.OJ.train, pretty = FALSE)

# e
tree.pred <- predict(object = tree.OJ.train, newdata = OJ.test, 
                     type = "class")
conf.mat <- table(OJ.test$Purchase, tree.pred)
# test error rate
sum(conf.mat[2,1], conf.mat[1,2]) / sum(conf.mat)

# f
cv.OJ.tree <- cv.tree(object = tree.OJ.train, FUN = prune.misclass)

# g
plot(x = cv.OJ.tree$size, y = cv.OJ.tree$dev, 
     xlab = "size", ylab = "Error Rate(CV)")

# h
# The tree with 5 terminal nodes

# i
tree.OJ.prune <- prune.misclass(tree = tree.OJ.train, best = 4)

# j
summary(tree.OJ.prune)
# Training errors are the same

# k
tree.pred.prune <- predict(object = tree.OJ.prune, 
                           newdata = OJ.test, type = "class")
conf.mat.prune <- table(OJ.test$Purchase, tree.pred.prune)
sum(conf.mat.prune[2,1], conf.mat.prune[1,2]) / sum(conf.mat.prune)
# Test errors are the same














