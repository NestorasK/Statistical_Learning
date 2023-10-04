# Exercise 13.
# Repeat the analysis of Lab 10.9.5 on the IMDb data using a similarly
# structured neural network. There we used a dictionary of size 10,000.
# Consider the effects of varying the dictionary size. Try the values
# 1000, 3000, 5000, and 10,000, and compare the results.


rm(list = ls())
library(httpgd)
# hgd()
# hgd_browse()
library(keras)

max_features <- c(1000, 3000, 5000, 10000)

for (i in seq_len(length.out = length(max_features))) {
    max_featurei <- max_features[i]

    imdb <- dataset_imdb(num_words = max_featurei)
    c(c(x_train, y_train), c(x_test, y_test)) %<-% imdb

    library(Matrix)
    one_hot <- function(sequences, dimension) {
        seqlen <- sapply(sequences, length)
        n <- length(seqlen)
        rowind <- rep(1:n, seqlen)
        colind <- unlist(sequences)
        sparseMatrix(
            i = rowind, j = colind,
            dims = c(n, dimension)
        )
    }
    x_train_1h <- one_hot(x_train, max_featurei)
    x_test_1h <- one_hot(x_test, max_featurei)
    dim(x_train_1h)
    nnzero(x_train_1h) / (25000 * max_featurei)

    set.seed(3)
    ival <- sample(seq(along = y_train), 2000)

    model <- keras_model_sequential() %>%
        layer_dense(
            units = 16, activation = "relu",
            input_shape = c(max_featurei)
        ) %>%
        layer_dense(units = 16, activation = "relu") %>%
        layer_dense(units = 1, activation = "sigmoid")
    model %>% compile(
        optimizer = "rmsprop",
        loss = "binary_crossentropy", metrics = c("accuracy")
    )
    history <- model %>% fit(x_train_1h[-ival, ], y_train[-ival],
        epochs = 20, batch_size = 512,
        validation_data = list(x_train_1h[ival, ], y_train[ival])
    )
    print(
        plot(history) +
            ggplot2::ylim(c(0, 1)) +
            ggplot2::ggtitle(max_featurei)
    )
}
