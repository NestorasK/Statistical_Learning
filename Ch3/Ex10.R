rm(list = ls())
library(ISLR)
data("Carseats")

# (a)
lmFit <- lm(formula = Sales ~ Price + Urban + US, data = Carseats)

# (b)
summary(lmFit)
# The Price is presents a negative relation to Sales
# The higher the price the lower the sales
plot(Carseats$Price, Carseats$Sales)
# Urban area does not influence sales
plot(Carseats$Urban, Carseats$Sales)
# Stores in the US associated with higher sales
plot(Carseats$US, Carseats$Sales)

# (c)
contrasts(Carseats$Urban)
contrasts(Carseats$US)
# Sales = 13.043469 - 0.054459 * Price - 0.021916 * Urban + 1.200573 * US

# If Urban = YES and US = YES
# Sales = 13.043469 - 0.021916 + 1.200573 - 0.054459 * Price 
# Sales = 14.22213 - 0.054459 * Price 

# If Urban = YES and US = NO
# Sales = 13.043469 - 0.054459 * Price - 0.021916
# Sales = 13.043469 - 0.021916 - 0.054459 * Price 
# Sales = 13.02155 - 0.054459 * Price 

# If Urban = NO and US = YES
# Sales = 13.043469 - 0.054459 * Price + 1.200573
# Sales = 13.043469 + 1.200573 - 0.054459 * Price 
# Sales = 14.24404 - 0.054459 * Price 

# If Urban = NO and US = NO
# Sales = 13.043469 - 0.054459 * Price


# (d)
# Price and US

# (e)
lmFit2 <- lm(formula = Sales ~ Price + US, data = Carseats)
summary(lmFit2)

# (f)
# lmFit2 fits the data better which is evident from
# - Residual standard error - RSE: lmFit2 has a lower RSE than lmFit
# - Rsquare: lmFit2 has a higher Rsquare than lmFit
# Eventhough adding more variables could generate higher Rsquare, this is not the case here providing more evidence that Urban variable provide no extra information to the model.

# (g)
confint(object = lmFit2)

# (h)
plot(x = hatvalues(model = lmFit2), y = rstudent(model = lmFit2), 
     xlab = "Leverage", ylab = "Studentized Residuals")
# From the plot is evident that there are observations with high Leverage
# Observations with high absolute value, close to 3, of Studentized Residuals could be possible outliers












