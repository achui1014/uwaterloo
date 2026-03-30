# practice assignment 3
hp <- read.table("hp.txt", header = TRUE) # house price data

# a) Fit the linear regression model:
# Price = B_0 + B_1(Size) + B_2(Beds) + B_3(Baths) + B_4(New) + B_5(Taxes) # nolint
fit <- lm(Price ~ Size + Beds + Baths + New + Taxes, data = hp)
summary(fit)

# Examine standardized residuals vs. fitted values
quartz()
plot(fitted(fit), rstandard(fit),
     xlab = "Fitted Values",
     ylab = "Standardized Residuals")

qqnorm(rstandard(fit))
qqline(rstandard(fit), col = "blue", lwd = 2)

# b) box-cox transformation
library(MASS)
bc <- boxcox(fit, lambda = seq(-1, 1, 0.05))
lambda_opt <- bc$x[which.max(bc$y)]
bc
abline(v = 0.5, lty = 3)

# c) refit the regression model using the transformed response (box-cox)
fit_log <- lm(log(Price) ~ Size + Beds + Baths + New + Taxes, data = hp)
summary(fit_log)

# Re-examine standardized residuals vs. fitted values
quartz()
plot(fitted(fit_log), rstandard(fit_log),
     xlab = "Fitted Values",
     ylab = "Standardized Residuals")

qqnorm(rstandard(fit_log))
qqline(rstandard(fit_log), col = "blue", lwd = 2)

# d) identify the most important predictors using backward variable selection
# iteration 1: remove Beds
back_fit <- lm(log(Price) ~ Size + Baths + New + Taxes, data = hp)
summary(back_fit)

# iteration 2: remove Baths
back_fit <- lm(log(Price) ~ Size + New + Taxes, data = hp)
summary(back_fit)