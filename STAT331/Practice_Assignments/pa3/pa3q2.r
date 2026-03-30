# stat331: pa3q2
data <- read.table("data/salary.txt", header = TRUE)

fit <- lm(salary ~ factor(degree) + experience + n_supervisees, data = data)
summary(fit)

# Plot of Residuals vs. Fitted Values
plot(fitted(fit), residuals(fit),
    xlab = "Fitted Values",
    ylab = "Residuals")

# Plot of Absolute Residuals vs. Experience
plot(data$experience, abs(residuals(fit)),
    xlab = "Experience",
    ylab = paste("|", expression(r[i]), "|"))

# Plot of Absolute Residuals vs. N_Supervisees
plot(data$n_supervisees, abs(residuals(fit)),
    xlab = "n_supervisees",
    ylab = paste("|", expression(r[i]), "|"))

# Model the variance:
# Regress absolute residuals on experience and n_supervisees and compute
# weights for each observation
var_fit <- lm(abs(residuals(fit)) ~ experience + n_supervisees, data = data)
summary(var_fit)

w <- 1 / (fitted(var_fit)^2)

# Fit WLS using weights w
wls_fit <- lm(salary ~ factor(degree) + experience + n_supervisees,
    data = data, weights = w)
summary(wls_fit)

# Plot of residuals vs. fitted values from the WLS model
plot(fitted(wls_fit), residuals(wls_fit),
    xlab = "Fitted Values",
    ylab = "Residuals",
    main = "Residuals vs. Fitted values (WLS Model)")
