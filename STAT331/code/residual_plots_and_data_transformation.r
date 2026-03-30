# Residual Plots and Data Transformation
pairs(~ Volume + Girth + Height, data = trees)

# fit a linear regression model:
# Volume = beta_0 + beta_1(Girth) + beta_2(Height) + epsilon
fit1 <- lm(Volume ~ Girth + Height, trees)
summary(fit1)

# Residual plots:
# Residual vs. Fitted
plot(fitted(fit1), residuals(fit1), ylim = c(-9, 9),
    xlab = "Fitted", ylab = "Residual")

# Normal Q-Q plot of standardized residuals
qqnorm(rstandard(fit1))
qqline(rstandard(fit1), col = "blue", lwd = 2)

# Residual vs. Girth
plot(trees$Girth, residuals(fit1), ylim = c(-9, 9),
    xlab = "Girth (inches)", ylab = "Residual")

# Residual vs. Height
plot(trees$Height, residuals(fit1), ylim = c(-9, 9),
    xlab = "Height (ft)", ylab = "Residual")

# Linear Regression with Quadratic Girth Term
# Volume = beta_0 + beta_1(Girth) + beta_2(Girth^2) + beta_3(Height) + epsilon
trees$Girth2 <- trees$Girth^2
fit2 <- lm(Volume ~ Girth + Girth2 + Height, data = trees)
summary(fit2)

# Residual plots after adding Girth^2
# Residual vs. Fitted
plot(fitted(fit2), residuals(fit2), ylim = c(-9, 9),
    xlab = "Fitted", ylab = "Residual")

# Normal Q-Q plot of standardized residuals
qqnorm(rstandard(fit2))
qqline(rstandard(fit2), col = "blue", lwd = 2)

# Residual vs. Girth
plot(trees$Girth2, residuals(fit2), ylim = c(-9, 9),
    xlab = "Girth (inches)", ylab = "Residual")

# Residual vs. Height
plot(trees$Height, residuals(fit2), ylim = c(-9, 9),
    xlab = "Height (ft)", ylab = "Residual")

# -----------------------------------------------------------------------------
# Example 2: Car Price Dataset
library(MASS)
data(Cars93)

# Histogram of car prices
hist(Cars93$Price, breaks = 10, col = "lightblue",
    border = "black",
    main = "Histogram of Car Price",
    xlab = "Car Prices")

# Fit: Price = beta_0 + beta_1(Horsepower) + beta_2(EngineSize) + epsilon
fit1 <- lm(Price ~ Horsepower + EngineSize, data = Cars93)
summary(fit1)

par(mfrow = c(1, 2))
#Standardized Residual vs. Fitted
plot(fitted(fit1), rstandard(fit1), ylim = c(-4, 4),
    xlab = "Fitted Values",
    ylab = "Standardized Residuals")

# Normal Q-Q Plot of Standardized Residuals
qqnorm(rstandard(fit1))
qqline(rstandard(fit1), col = "blue", lwd = 2)

# Box-Cox plot
boxcox(fit1, lambda = seq(-1, 1, 0.05))

bc <- boxcox(fit1, lambda = seq(-1, 1, 0.05), plotit = FALSE)
lambda_opt <- bc$x[which.max(bc$y)]
lambda_opt

# optimal lambda from box-cox is lambda_op = -0.2
# price^(-0.2) = beta_0 + beta_1(Horsepower) + beta_2(EngineSize) + epsilon
# since the confidence interval for lambda inlcudes 0, the log transformation
# is reasonable, and we consider the model:
# log(Price) = beta_0 + beta_1(Horsepower) + beta_2(EngineSize) + epsilon

fit_log <- lm(log(Price) ~ Horsepower + EngineSize, data = Cars93)
summary(fit_log)

# Residual plots after Log Transformation
# Stanndardized Residual vs. Fitted
plot(fitted(fit_log), rstandard(fit_log), ylim = c(-4, 4),
    xlab = "Fitted", ylab = "Standardized Residual")

# Normal Q-Q plot of standardized residuals
qqnorm(rstandard(fit_log))
qqline(rstandard(fit_log), col = "blue", lwd = 2)