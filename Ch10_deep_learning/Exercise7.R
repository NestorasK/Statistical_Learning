# Exercise 7.
# Fit a neural network to the Default data. Use a single hidden layer
# with 10 units, and dropout regularization. Have a look at Labs 10.9.1–
# 10.9.2 for guidance. Compare the classification performance of your
# model with that of linear logistic regression.


rm(list = ls())
library(httpgd)
library(caret)
hgd()
hgd_browse()

library(ISLR2)
library(keras)

data("Default")
Default$balance <- scale(Default$balance)
Default$income <- scale(Default$income)
default_data <- model.matrix(default ~ . - 1, data = Default)

inds <- caret::createFolds(y = Default$default, k = 10, list = FALSE)

x_train <- default_data[inds != 1, ]
y_train <- Default[inds != 1, c("default")]
y_train <- to_categorical(y = (as.numeric(y_train) - 1), num_classes = 2)

x_test <- default_data[inds == 1, ]
y_test <- Default[inds == 1, c("default")]
y_test <- as.numeric(y_test) - 1

modnn <- keras_model_sequential() %>%
    layer_dense(
        units = 10,
        activation = "relu",
        input_shape = ncol(x_train)
    ) %>%
    layer_dropout(rate = 0.4) %>%
    layer_dense(units = 2, activation = "softmax")

summary(modnn)

modnn %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_rmsprop(),
    metrics = "accuracy"
)
history <- modnn %>%
    fit(x_train, y_train,
        epoch = 60, batch_size = 128,
        validation_split = 0.2
    )
plot(history, smooth = FALSE)

nn_predictions <- predict(modnn, x_test) %>%
    k_argmax() %>%
    as.numeric()

table(truth = y_test, pred = nn_predictions)
mean(y_test == nn_predictions)

# Linear logistic regression
summary(Default)
glm_fit <- glm(
    formula = default ~ student + balance + income,
    family = "binomial",
    data = Default[inds != 1, ]
)
glm_predictions <- predict(glm_fit, Default[inds == 1, ], type = "response")
glm_pred_class <- ifelse(test = glm_predictions > 0.5, yes = 1, no = 0)
table(truth = y_test, glm_pred = glm_pred_class)
mean(y_test == glm_pred_class)

# baseline - Assuming we set everything to 0
table(y_test)[1] / length(y_test)
