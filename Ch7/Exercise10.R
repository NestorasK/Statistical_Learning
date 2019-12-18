# Exercise 10, Ch 7, pg 300
rm(list = ls())
library(ISLR)
library(leaps)
library(gam)
data("College")
set.seed(123)
# Adopted from lab - chapter 6
predict.regsubsets=function(object,newdata,id,...){
    form=as.formula(object$call[[2]])
    mat=model.matrix(form,newdata)
    coefi=coef(object,id=id)
    xvars=names(coefi)
    mat[,xvars]%*%coefi
}

# a
# Train - test split
nfolds_out <- 10
folds_out <- sample(x = 1:nfolds_out, size = nrow(College), replace = TRUE)

# Forward step selection 
# Identify the best number of features performing 5 fold cross validation
# on the training set
college_train <- College[ folds_out != 1, ]
folds_in <- sample(x = 1:nfolds_out, size = nrow(college_train), replace = TRUE)
cverrors <- matrix(data = NA, nrow = nfolds_out, ncol = ncol(college_train) - 1)
rownames(cverrors) <- paste("fold", 1:nfolds_out, sep = ".")
colnames(cverrors) <- paste("numFeatures", 1:(ncol(college_train) - 1), sep = ".")

for(fold_i in 1:nfolds_out){
    regfit.fwd_i <- regsubsets(Outstate ~., data = college_train[ folds_in != fold_i, ], 
                               method = "forward", nvmax = (ncol(College) - 1))
    for(j in 1:(ncol(college_train) - 1)){
        pred_i <- predict.regsubsets(object = regfit.fwd_i,
                                     newdata = college_train[folds_in == fold_i, ], 
                                     id = j)
        mse_i <- mean( ( pred_i - college_train[folds_in == fold_i, "Outstate"] )^2 )
        cverrors[fold_i, j] <- mse_i
    }
}

mean.cv.error <- apply(X = cverrors, MARGIN = 2, FUN = mean)
sd.cv.error <- apply(X = cverrors, MARGIN = 2, FUN = sd)
boundaries <- cbind(mean.cv.error + sd.cv.error, mean.cv.error - sd.cv.error)

plot(x = mean.cv.error, xlab = "Number of features", main = "10 fold CV results", 
     ylim = c(min(boundaries), max(boundaries)))
points(x = which.min(mean.cv.error), y = mean.cv.error[which.min(mean.cv.error)], 
       pch = 19, col = "red")
lines(x = 1:17, y = rep(mean.cv.error[which.min(mean.cv.error)] + 
                            sd.cv.error[which.min(mean.cv.error)], 17),
      lty = 2, col = "red")
lines(x = 1:17, y = rep(mean.cv.error[which.min(mean.cv.error)] - 
                            sd.cv.error[which.min(mean.cv.error)], 17), 
      lty = 2, col = "red")
points(x = 5, y = mean.cv.error[5], pch = 19, col = "blue")
legend(x = "topright", legend = c("Best num of features", 
                                  "Equivalent to best", 
                                  "Everything else"), 
       pch = c(19, 19, 1), col = c("red", "blue" , "black"))

# Identify the features the full training set
regfit.fwd.best <- regsubsets(Outstate ~., data = college_train, 
                              method = "forward", nvmax = 5)
selected_features <- coef(regfit.fwd.best, id = 5)[-1]


# b TODO ####
college_train.mm <- as.data.frame(model.matrix(object = Outstate ~ ., 
                                            data = college_train)[,-1])
college_train_selFeat <- college_train.mm[, selected_features]
college_train_selFeat$Outstate <- college_train$Outstate

fit.gam <- gam(formula = Outstate ~ ., data = college_train_selFeat)

plot(fit.gam)
















