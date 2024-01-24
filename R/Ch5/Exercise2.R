# Exercise 2, pg 197
rm(list = ls())

probInBootstrap <- function(n){
    out <- 1 - ( (1 - 1/n)^n )
    return(out)
}

# d
probInBootstrap(n = 5)

# e
probInBootstrap(n = 100)

# f
probInBootstrap(n = 10000)

# g
probs <- sapply(X = c(1:100000), FUN = probInBootstrap)
summary(probs)
plot(probs)

# h
store <- rep(NA, 10000)
for(i in 1:10000){
    store[i] <- sum(sample(x = 1:100, size = 100, replace = TRUE) == 4) > 0
}
mean(store)
