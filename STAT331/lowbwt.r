
data <- read.table("data/lowbwt.txt", header = TRUE)

# Calculate the least squares estimate
y <- data$headcirc
x <- data$gestage
xbar <- mean(x)
ybar <- mean(y)
Sxy <- sum((x - xbar) * (y - ybar))
Sxx <- sum((x - xbar)^2)

b1hat <- Sxy/Sxx
b0hat <- ybar - b1hat * xbar

# Fit the simple linear model
fit1 <- lm(headcirc ~ gestage, data = data)
summary(fit1)$coefficients
summary(fit1)$sigma^2

# Fitted values
muhat <- b0hat + b1hat * x

# Residuals
r <- y - muhat

# Lecture 6 Examples
# 1. What range likely contians the average headcirc for infants with
# gestage of 30 weeks?
# (use 95% CI for mean response at x* = 30)
newdat <- data.frame(gestage = 30)
predict(fit1, newdat, interval = "confidence", level = 0.95)

# 2. Where will the headcirc for a future infant born at 30 weeks gestage
# likely fall?
# (use a 95% prediction interval at x* = 30)
predict(fit1, newdat, interval = "prediction", level = 0.95)