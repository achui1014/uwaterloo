library(readxl)

spring <- read.table("spring.txt", header = TRUE)

num <- sum(spring$Force * spring$Length)
denom <- sum(spring$Force^2)
beta_hat <- num/denom

pdf("spring_plot.pdf", width = 7, height = 5)
plot(spring$Force, spring$Length,
    main = "Scatterplot of spring force vs. length",
    xlab = "Force (pounds)",
    ylab = "Length (inches)",
    pch = 19,
    ylim = c(0, 18),
    xlim = c(0, 18)
)

abline(a = 0, b = beta_hat, col = "red", lwd = 2)
dev.off()

tesla <- read_excel("tesla.xlsx")

pdf("stock_plot.pdf", width = 7, height = 5)
x <- tesla$"R(S&P 500, t)%"
y <- tesla$"R(TSLA, t)%"
plot(x, y,
    main = "Scatterplot of Tesla vs. S&P500 Stock Return (Feb 2019 - Dec 2023)",
    xlab = "Monthly Rate of Return of Tesla Stock (%)",
    ylab = "Rate of Return of S&P500 (%)",
    pch = 19)
dev.off()

# Fitting the SLR model
num <- sum((x - mean(x)) * (y - mean(y)))
denom <- sum((x - mean(x))^2)
beta_one_hat <- num/denom

beta_zero_hat <- mean(y) - (beta_one_hat * mean(x))

abline(beta_zero_hat, beta_one_hat, col = "red", lwd = 2)
dev.off()

# 95% CI for estimate of Beta1
fit <- lm(y ~ x)
confint(fit, level = 0.95)
summary(fit)

# H_0: Beta_1 = 1
b1  <- coef(fit)["x"]
se1 <- summary(fit)$coefficients["x", "Std. Error"]
t_stat <- (b1 - 1) / se1
p_value <- 2 * (1 - pt(abs(t_stat), fit$df.residual))

# Expected return when market return is 5%
# New market return value
new_x <- data.frame(x = 0.05)

# 95% confidence interval for the mean response
predict(fit, newdata = new_x, interval = "confidence", level = 0.95)

#95% prediction interval
predict(fit, newdata = new_x, interval = "prediction", level = 0.95)