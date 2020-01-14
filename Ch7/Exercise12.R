rm(list = ls())
set.seed(123)

# Some norm data
mymat <- matrix(data = rnorm(n = 100*1000), nrow = 1000, ncol = 100)
y <- rnorm(n = 1000)

# Initialize betas table
nIterations <- 20
betas <- matrix(data = NA, nrow = nIterations, ncol = ncol(mymat))
rownames(betas) <- paste("Iter", 1:nIterations, sep = ".")
colnames(betas) <- c(paste("beta", 1:ncol(mymat), sep = "."))
beta0 <- rep(NA, times = nIterations)

# Initialize all betas of the first iteration but one
betas[1, ] <- c(NA, rnorm(n = ncol(betas)-1))

# First iterations
for(j in 1:ncol(mymat)){
    a <- y - mymat[, -j] %*% betas[1, -j] 
    betas[1, j] <- lm(a ~ mymat[, j])$coef[2]
}
beta0[1] <- lm(a ~ mymat[, j])$coef[1]

for(i in 2:nIterations){
    cat(i, "\n")
    for(j in 1:ncol(mymat)){
        a <- y - mymat[, -j] %*% betas[i - 1, -j] 
        betas[i, j] <- lm(a ~ mymat[, j])$coef[2]
    }
    beta0[i] <- lm(a ~ mymat[, j])$coef[1]
    
}

# Regular Fitting
fit.lm <- lm(y ~ mymat)

# Compare coefficients
plot(x = coef(fit.lm), y = c(beta0[nIterations], betas[nIterations, ]),
     xlab = "Multiple Regression coefs", ylab = "Backfitting coefs", 
     main = paste("Cor", round(x = cor(x = coef(fit.lm), 
                                       y = c(beta0[nIterations],
                                             betas[nIterations, ])),
                               digits = 2)))

# Necessary steps
# Mean differences from the truth - lm output
betas <- cbind(beta0, betas)
MSE_Diffs <- apply(X = betas, MARGIN = 1, FUN = function(ri){
    return(mean((coef(fit.lm) - ri)^2))
})

LS_Diffs <- apply(X = betas, MARGIN = 1, FUN = function(ri){
    return(sqrt( sum( ( ri - coef(fit.lm) )^2 ) ))
})
    
    


plot(MSE_Diffs, xlab = "Number of iterations", ylab = "Mean Square Error", 
     main = "Mean differences from the truth - lm output")

plot(LS_Diffs, xlab = "Number of iterations", ylab = "LS diff", 
     main = "Mean differences from the truth - lm output")
