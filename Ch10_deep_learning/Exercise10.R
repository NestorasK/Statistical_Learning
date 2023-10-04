# Exercise 10.
# In Section 10.9.6, we showed how to fit a linear AR model to the
# NYSE data using the lm() function. However, we also mentioned that
# we can “flatten” the short sequences produced for the RNN model in
# order to fit a linear AR model. Use this latter approach to fit a linear
# AR model to the NYSE data. Compare the test R2 of this linear AR
# model to that of the linear AR model that we fit in the lab. What
# are the advantages/disadvantages of each approach?

rm(list = ls())
library(httpgd)
# hgd()
# hgd_browse()
library(keras)
library(ISLR2)
xdata <- data.matrix(
    NYSE[, c("DJ_return", "log_volume", "log_volatility")]
)
istrain <- NYSE[, "train"]
xdata <- scale(xdata)

lagm <- function(x, k = 1) {
    n <- nrow(x)
    pad <- matrix(NA, k, ncol(x))
    rbind(pad, x[1:(n - k), ])
}

arframe <- data.frame(
    log_volume = xdata[, "log_volume"],
    L1 = lagm(xdata, 1), L2 = lagm(xdata, 2),
    L3 = lagm(xdata, 3), L4 = lagm(xdata, 4),
    L5 = lagm(xdata, 5)
)

arframe <- arframe[-(1:5), ]
istrain <- istrain[-(1:5)]

n <- nrow(arframe)
xrnn <- data.matrix(arframe[, -1])
xrnn <- array(xrnn, c(n, 3, 5))
xrnn <- xrnn[, , 5:1]
xrnn <- aperm(xrnn, c(1, 3, 2))

# model <- keras_model_sequential() %>%
#     layer_simple_rnn(
#         units = 12,
#         input_shape = list(5, 3),
#         dropout = 0.1, recurrent_dropout = 0.1
#     ) %>%
#     layer_dense(units = 1)


model <- keras_model_sequential() %>%
    layer_flatten(input_shape = c(5, 3)) %>%
    layer_dense(units = 1)


model %>% compile(
    optimizer = optimizer_rmsprop(),
    loss = "mse"
)

history <- model %>% fit(
    xrnn[istrain, , ], arframe[istrain, "log_volume"],
    batch_size = 64, epochs = 200,
    validation_data =
        list(xrnn[!istrain, , ], arframe[!istrain, "log_volume"])
)
plot(history)
V0 <- var(arframe[!istrain, "log_volume"])
kpred <- predict(model, xrnn[!istrain, , ])
1 - mean((kpred - arframe[!istrain, "log_volume"])^2) / V0

# Fitting the linear model for comparison
arfit <- lm(log_volume ~ ., data = arframe[istrain, ])
arpred <- predict(arfit, arframe[!istrain, ])
1 - mean((arpred - arframe[!istrain, "log_volume"])^2) / V0
