load('Data/7.R.RData')
plot(x = x, y = y)

fit <- lm(formula = y ~ x, data = data.frame(x, y))
abline(fit)
summary(fit)


fit2 <- lm(formula = y ~ 1 + x + I(x = x^2))

fit2.2 <- lm(formula = y ~ 1 + poly(x, 2))



preds <- predict(object = fit2, newdata = data.frame(x))

plot(x = x, y = y)
points(x = x, y = preds, col = 'green')


summary(fit2)
plot(fit2)
2

