# Exercise 9
# Fit a lag-5 autoregressive model to the NYSE data, as described in
# the text and Lab 10.9.6. Refit the model with a 12-level factor
# representing the month. Does this factor improve the performance of the
# model?

rm(list = ls())
library(httpgd)
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

# Add month
months <- factor(
    x = sapply(
        X = strsplit(
            x = NYSE$date[-(1:5)], split = "-", fixed = TRUE
        ),
        FUN = function(li) {
            li[[2]]
        }
    ),
    ordered = TRUE
)
arframed <- data.frame(
    month = months,
    day = NYSE[-(1:5), "day_of_week"],
    arframe
)
x <- model.matrix(log_volume ~ . - 1, data = arframed)
colnames(x)

arnnd <- keras_model_sequential() %>%
    layer_dense(
        units = 32, activation = "relu",
        input_shape = ncol(x)
    ) %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 1)
arnnd %>% compile(
    loss = "mse",
    optimizer = optimizer_rmsprop()
)
history <- arnnd %>% fit(
    x[istrain, ], arframed[istrain, "log_volume"],
    epochs = 100,
    batch_size = 32, validation_data =
        list(x[!istrain, ], arframed[!istrain, "log_volume"])
)
plot(history)

V0 <- var(arframed[!istrain, "log_volume"])
npred <- predict(arnnd, x[!istrain, ])
1 - mean((arframed[!istrain, "log_volume"] - npred)^2) / V0
