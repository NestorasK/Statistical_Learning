# Exercise 3, Ch8
rm(list = ls())
classificationError <- function(pm1){
    pm2 <- 1 - pm1
    Error <- 1 - max(c(pm1, pm2))
    return(Error)
}
giniIndex <- function(pm1){
    pm2 <- 1 - pm1
    giniInd <- sum(c(pm1*(1 - pm1),pm2*(1 - pm2)))
    return(giniInd)
}

entropy <- function(pm1){
    pm2 <- 1 - pm1
    D <- -(sum(c( pm1 * log(pm1), 
                  pm2 * log(pm2))  
               ))
    return(D)
}

pm1_i <- seq(from = 0, to = 1, by = 0.01)
classError <- sapply(X = pm1_i, FUN = classificationError)
giniInds <- sapply(X = pm1_i, FUN = giniIndex)
entr  <- sapply(X = pm1_i, FUN = entropy)
outs <- data.frame(classError, giniInds, entr)

plot(x = pm1_i, y = classError, type = "n", ylim = range(outs, na.rm = TRUE), 
     ylab = "Metric")
lines(x = pm1_i, y = classError, col = "green")
lines(x = pm1_i, y = giniInds, col = "red")
lines(x = pm1_i, y = entr, col = "blue")
legend("bottom", legend = c("Classification Error", "GiniIndex", "Entropy"), 
       col = c("green", "red", "blue"), lty = 1)
