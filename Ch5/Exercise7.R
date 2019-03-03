rm(list = ls())
library(boot)
library(ISLR)
data(Weekly)

# a
fit.glm <- glm(formula = Direction ~ Lag1 + Lag2, family = "binomial", data = Weekly)


# b
indexi <- 1
fit.glm_i <- glm(formula = Direction ~ Lag1 + Lag2, family = "binomial", 
                 data = Weekly[-indexi , ])

# c
prodi <- predict.glm(object = fit.glm_i, newdata = Weekly[indexi, c("Lag1", 'Lag2')], 
                     type = "response")
if(prodi > 0.5){
    Directioni <- "Up"
}else{
    Directioni <- "Down"
}

# d
Directions.pred <- rep(NA, times = nrow(Weekly)) 
for(indexi in 1:nrow(Weekly)){
    fit.glm_i <- glm(formula = Direction ~ Lag1 + Lag2, family = "binomial", 
                     data = Weekly[-indexi , ])
    Directions.pred[indexi] <- ifelse(test = predict.glm(object = fit.glm_i, 
                                       newdata = Weekly[indexi, ], 
                                       type = "response") > 0.5, 
                    yes = "Up", no = "Down")
    

}

# e
Directions.pred <- factor(Directions.pred)
mean(Directions.pred != Weekly[, "Direction"])


# Evaluate if my output above is correct
cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)
test <- cv.glm(data = Weekly[, c("Lag1", "Lag2", "Direction")], cost = cost, 
               glmfit = fit.glm)

test$delta[1]




