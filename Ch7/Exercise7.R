rm(list = ls())
data("Wage")
library(ISLR)
library(boot)

# Plots
boxplot(formula = wage ~ maritl, data = Wage)
boxplot(formula = wage ~ jobclass, data = Wage)

# Fitting models
fit_i <- lm(formula = wage ~ maritl, data = Wage)
fit_ii <- lm(formula = wage ~ jobclass, data = Wage)
fit_iii <- lm(formula = wage ~ maritl + jobclass, data = Wage)

anova(fit_i, fit_iii)
anova(fit_ii, fit_iii)





