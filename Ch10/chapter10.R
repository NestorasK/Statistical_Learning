rm(list = ls())

# 10.R.1
load('Data/10.R.RData')

df <- rbind(x, x.test)

pcaout <- prcomp(x = df, scale. = TRUE)

plot(pcaout$x[,c(1,2)])

summar <- summary(pcaout)
sum(summar$importance[2, c(1:5)])

# 10.R.2
PCs1to5_train <- pcaout$x[1:300, 1:5]
df_train <- data.frame(cbind(y, PCs1to5_train))

m1 <- lm(formula = y ~ ., data = df_train)

m1.pred <- predict(object = m1, newdata = data.frame(pcaout$x[301:1300, 1:5]))

# Mean square error
mean((m1.pred - y.test)^2)


# 10.R.3
df_trainOLS <- data.frame(y, x)
m2 <- lm(formula = y ~ ., data = df_trainOLS)

m2.pred <- predict(object = m2, newdata = x.test)

mean(x = (m2.pred - y.test)^2)





