rm(list = ls())
set.seed(123)

# a
y <- rnorm(n = 100)
x1 <- rnorm(n = 100)
x2 <- rnorm(n = 100)

# b
beta1 <- 5

results <- matrix(data = NA, nrow = 1000, ncol = 4)
rownames(results) <- paste("Rep", 1:1000, sep = ".")
colnames(results) <- c("beta01", "beta02", "beta1", "beta2")

# e
for(i in 1:1000){
    
    # c
    a <- y - beta1*x1 
    beta2 <- lm(a ~ x2)$coef[2]
    beta02 <- lm(a ~ x2)$coef[1]
    
    # d
    a <- y - beta2*x2 
    beta1 <- lm(a ~ x1)$coef[2]
    beta01 <- lm(a ~ x1)$coef[1]
    
    results[i, ] <- c(beta01, beta02, beta1, beta2)
    
}
plot(x = 1:1000, y = results[, 1], type = "n", 
     xlab = "Repetitions", ylab = "Estimates",
     ylim = range(results))
lines(x = 1:1000, y = results[, 1], col = "black")
lines(x = 1:1000, y = results[, 3], col = "blue")
lines(x = 1:1000, y = results[, 4], col = "green")
legend(x = "topright", legend = c("b0", "beta1", "beta2"), 
       lty = 1, col = c("black", "blue", "green"))

# f
fit.lm <- lm(y ~ x1 + x2)
truth <- coef(fit.lm)
abline(h = truth[1], col = "grey")
abline(h = truth[2], col = "red")
abline(h = truth[3], col = "darkgreen")

# g
# 4









