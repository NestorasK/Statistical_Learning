rm(list = ls())
library(leaps)
set.seed(1)

# (a)
# Generate dataset
n = 1000
p = 20
mydataset <- data.frame(matrix(data = rnorm(n = n * p), nrow = n, ncol = p))
error <- rnorm(n = p)
# numNonZero <- 10
# betas <- c(rnorm(n = numNonZero, mean = 0), rep(x = 0, times = (20 - numNonZero)))
betas <- rnorm(p)
betas[3] = 0
betas[4] = 0
betas[9] = 0
betas[19] = 0
betas[10] = 0
mydataset.tmp <- t(t(mydataset) * betas)
mydataset$Y <- (apply(X = mydataset.tmp, MARGIN = 1, FUN = sum)) + error

# (b)
train.index <- sample(x = 1:nrow(mydataset), size = 100)
test.index <- -train.index
trainSet <- mydataset[train.index, ]
testSet <- mydataset[test.index, ]
trainY <- mydataset$Y[train.index]
testY <- mydataset$Y[test.index]

# (c)
predict.regsubsets=function(object,newdata,id,...){
    form=as.formula(object$call[[2]])
    mat=model.matrix(form,newdata)
    coefi=coef(object,id=id)
    xvars=names(coefi)
    mat[,xvars]%*%coefi
}

# Fit best subset selection
fit.bss <- regsubsets(x = Y ~ ., data = trainSet, nvmax = p)
summary(fit.bss)
# Train MSEs
train.MSEs <- rep(NA, p)
for(i in 1:p){
    pred_i <- predict.regsubsets(object = fit.bss, newdata = trainSet, id = i)
    train.MSEs[i] <- mean((pred_i - trainY)^2)
}
plot(train.MSEs, type = "b", xlab = "Number of features")

# (d)
# Test MSEs
test.MSEs <- rep(NA, p)
for(i in 1:p){
    pred_i <- predict.regsubsets(object = fit.bss, newdata = testSet, id = i)
    test.MSEs[i] <- mean((pred_i - testY)^2)
}
plot(test.MSEs, type = "b", xlab = "Number of features")

# (e)
which.min(test.MSEs)

# (f)
coef(object = fit.bss, id = which.min(test.MSEs))
betas

# (g)
Coefficient.error <- rep(NA, p)
Number_of_coef <- rep(NA, p)
for(i in 1:p){
    coefi <- coef(object = fit.bss, id = i)[-1]
    Number_of_coef[i] <- length(coefi)
    cat(Number_of_coef[i], "----", coefi, "\n")
    inds <- which(colnames(mydataset) %in% names(coefi))
    Coefficient.error[i] <- sqrt(sum((coefi - betas[inds])^2))
}
plot(x = Number_of_coef, y = Coefficient.error, xlab = "Number of Coefficients",
     ylab = "Coefficients error")
points(x = which.min(Coefficient.error), 
       y = Coefficient.error[which.min(Coefficient.error)], 
       pch = 19, col = "red")
which.min(Coefficient.error)




