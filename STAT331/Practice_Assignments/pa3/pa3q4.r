# stat331: pa3q4
rain_dat <- read.table("data/rainseeding.txt", header = TRUE)

# use backward elimination algorithm to select an appropriate regression model
# for rainfall
fit0 <- lm(Rain ~ SA + Time + EchoCov + EchoMot + PreWet, data = rain_dat)
summary(fit0) # remove EchoCov
fit1 <- lm(Rain ~ SA + Time + EchoMot + PreWet, data = rain_dat)
summary(fit1) # remove SA
fit2 <- lm(Rain ~ Time + EchoMot + PreWet, data = rain_dat)
summary(fit2) # remove PreWet
fit3 <- lm(Rain ~ Time + EchoMot, data = rain_dat)
summary(fit3)
# Time and EchoMot are statistically significant at the 0.05 level

# use all subset regression to evaluate each model
library(leaps)
# fit all possible models
regfit_full <- regsubsets(Rain ~ ., data = rain_dat)
reg_summary <- summary(regfit_full)

# Plot C_p
plot(reg_summary$cp, type = "b",
    xlab = "Number of predictors", ylab = expression(C_p),
    main = "C_p vs. # of Variables")

p <- 1:length(reg_summary$cp)
best <- which.min(abs(reg_summary$cp - p))
text(best, reg_summary$cp[best],
    labels = round(reg_summary$cp[best], 3),
    pos = 3)

# find the index of the model with C_p ~= p
names(coef(regfit_full, best))[-1] # remove intercept

# Plot Adjusted R^2
plot(reg_summary$adjr2, type = "b",
    xlab = "Number of Predictors", ylab = "Adjusted R^2",
    main = "Adjusted R^2 vs. # of Variables")
abline(v = which.max(reg_summary$adjr2), col = "red", lty = 2)
text(which.max(reg_summary$adjr2), max(reg_summary$adjr2),
    labels = paste0("Max at ", which.max(reg_summary$adjr2)),
    pos = 1)

# find the index of the model with the maximum adjusted R^2
best_adjr2 <- which.max(reg_summary$adjr2)
names(coef(regfit_full, best_adjr2))[-1] # remove intercept

# assess effectiveness of cloud seeding in selected models
fit_r2 <- lm(Rain ~ Time + SuitCr + EchoMot, data = rain_dat)
summary(fit_r2)

fit_cp <- lm(Rain ~ SA + Time + SuitCr + EchoMot + PreWet, data = rain_dat)
summary(fit_cp)

fit_be <- lm(Rain ~ Time + EchoMot, data = rain_dat)
summary(fit_be)

# Check for influential cases in the data and evaluate the sensitivity of your 
# results to these cases
# check using the backwards elimination model
r <- residuals(fit_be)
e <- rstandard(fit_be)
leverage <- hatvalues(fit_be)
cookd <- cooks.distance(fit_be)

plot(fit_be$fitted, e, xlab = "Fitted Values",
    ylab = "Standardized Residuals",
    main = "Standardized Resdiual vs. Fitted Values", cex = 1)
text(fit_be$fitted[abs(e) > 2], e[abs(e) > 2],
    labels = rain_dat$Rain[abs(e) > 2], cex = 0.75, pos = 2)

index <- 1:24
plot(index, leverage, ylab = "Leverage", main = "Leverage", cex = 1)
abline(h = 2 * mean(leverage), col = "red", lty = 2)
text(index[leverage > 2 * mean(leverage)],
    leverage[leverage > 2 * mean(leverage)],
    labels = rain_dat$Rain[leverage > 2 * mean(leverage)],
    cex = 0.75, pos = 1)

plot(index, cookd, ylab = "Cook's distance", main = "Cook's Distance", cex = 1)
text(index[cookd >= 0.5], cookd[cookd >= 0.5],
    labels = rain_dat$Rain[cookd >= 0.5], cex = 0.75, pos = 1)

refit <- lm(Rain ~ Time + EchoMot,
    data = subset(rain_dat, Rain != 12.85))
round(summary(refit)$coefficients, 3)