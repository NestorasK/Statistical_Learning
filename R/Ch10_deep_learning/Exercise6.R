# Exercise 6.
# Consider the simple function R(β) = sin(β) + β/10.
# (a) Draw a graph of this function over the range β ∈ [−6, 6].
# (b) What is the derivative of this function?
# (c) Given β0 = 2.3, run gradient descent to find a local minimum
# of R(β) using a learning rate of ρ = 0.1. Show each of β0, β1, . . .
# in your plot, as well as the final answer.
# (d) Repeat with β0 = 1.4.

rm(list = ls())
loss_function <- function(b) {
    out <- sin(b) + (b / 10)
    return(out)
}

# a
betas <- seq(from = -6, to = 6, by = 0.1)
loss <- loss_function(b = betas)
plot(x = betas, y = loss, type = "l")

# b
loss_function_derivative <- function(b) {
    return(cos(b) + (1 / 10))
}

# c
gradient_desc <- function(b, learn_rate, conv_threshold, n, max_iter) {
    derivative <- loss_function_derivative(b = b)
    cat("derivative input:", derivative, "\n")
    step_size <- derivative * learn_rate
    cat("step size", step_size, "\n")
    counter <- 0
    bs_out <- rep(x = NA, times = max_iter)

    while (abs(step_size) > conv_threshold) {
        b <- b - step_size
        cat("newb", b, "\n")
        derivative <- loss_function_derivative(b = b)
        cat("derivative", derivative, "\n")
        step_size <- derivative * learn_rate
        cat("step size", step_size, "\n")
        if (counter == max_iter) {
            break
        }
        counter <- counter + 1
        bs_out[counter] <- b
    }
    return(bs_out[!is.na(bs_out)])
}
betas_out <- gradient_desc(
    b = 2.3, learn_rate = 0.1,
    conv_threshold = 0.001, max_iter = 1000
)
points(x = betas_out, y = loss_function(b = betas_out), col = "red", pch = 1)

# d
betas_out <- gradient_desc(
    b = 1.4, learn_rate = 0.1,
    conv_threshold = 0.001, max_iter = 1000
)
plot(x = betas, y = loss, type = "l")
points(x = betas_out, y = loss_function(b = betas_out), col = "blue", pch = 1)
