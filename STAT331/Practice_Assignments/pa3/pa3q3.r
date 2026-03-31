# stat331: pa3q3
hospital_dat <- read.table("data/hospitalstay.dat", header = TRUE)

# fit mlr with length as response variable
fit <- lm(Length ~ Culture + Xray + Region + PatNum, data = hospital_dat)
summary(fit)

plot(fitted(fit), residuals(fit),
    xlab = "Fitted Values",
    ylab = "Residuals",
    main = "Fitted Values vs. Residuals")

qqnorm(rstandard(fit))
qqline(rstandard(fit), col = "blue", lwd = 2)

# identify outliers
r <- residuals(fit)
e <- rstandard(fit)
leverage <- hatvalues(fit)
cookd <- cooks.distance(fit)
head(data.frame(hospital_dat$Length, r = round(r, 3), e = round(e, 3),
    leverage = round(leverage, 3), CookD = round(cookd, 3)))

# Plot of Standardized residuals vs fitted values
plot(fit$fitted, e, xlab = "Fitted Values",
    ylab = "Standardized Residuals",
    main = "Standardized Residual vs. Fitted Values", cex = 1.5)
abline(h = c(2, -2), col = "red", lty = 2)
text(fit$fitted[abs(e) > 2], e[abs(e) > 2],
    labels = hospital_dat$Length[abs(e) > 2], cex = 0.75, pos = 2)

# Plot of Leverage Values
index <- 1:112
plot(index, leverage, ylab = "Leverage", main = "Leverage", cex = 1)
abline(h = 2 * mean(leverage), col = "red", lty = 2)
text(index[leverage > 2 * mean(leverage)],
    leverage[leverage > 2 * mean(leverage)],
    labels = hospital_dat$Length[leverage > 2 * mean(leverage)],
    cex = 0.75, pos = 1)

# Plot of Cook's distance
plot(index, cookd, ylab = "Cook's distance", main = "Cook's Distance", cex = 1)
text(index[cookd >= 0.5], cookd[cookd >= 0.5],
    labels = hospital_dat$Length[cookd >= 0.5], cex = 0.75, pos = 1)

# Refit Model Excluding Influential Point: Length = 18.5
refit <- lm(Length ~ Culture + Xray + Region + PatNum,
    data = subset(hospital_dat, Length != 18.5))
round(summary(refit)$coefficients, 3)