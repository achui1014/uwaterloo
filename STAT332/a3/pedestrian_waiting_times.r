push_dat <- data.frame(
  pushes = c(
    rep(0, 7),
    rep(1, 10),
    rep(2, 10),
    rep(3, 5)
  ),
  waiting_time = c(
    38.14, 38.20, 38.31, 38.14, 38.29, 38.17, 38.20,
    38.28, 38.17, 38.08, 38.25, 38.18, 38.03, 37.95, 38.26, 38.30, 38.21,
    38.17, 38.13, 38.16, 38.30, 38.34, 38.34, 38.17, 38.18, 38.09, 38.06,
    38.14, 38.30, 38.21, 38.04, 38.37
  )
)
push_dat$pushes <- as.factor(push_dat$pushes)

# part a: ANOVA table
# ------------------------------------------------------------------------------
boxplot(waiting_time ~ pushes, data = push_dat)
anova(lm(waiting_time ~ pushes, data = push_dat))

# part c: mean waiting times for one push and three pushes + 95% CI (Bonferroni)
# ------------------------------------------------------------------------------
aov_tab <- anova(lm(waiting_time ~ pushes, data = push_dat))

mse <- aov_tab["Residuals", "Mean Sq"]
df <- aov_tab["Residuals", "Df"]

means <- tapply(push_dat$waiting_time, push_dat$pushes, mean)
ns <- tapply(push_dat$waiting_time, push_dat$pushes, length)

# Bonferonni critical value
alpha <- 0.05
g <- 2
tcrit <- qt(1 - alpha / (2 * g), df = df)

# one push
se_one <- sqrt(mse / ns["1"])
ci_one <- means["1"] + c(-1, 1) * tcrit * se_one

# three pushes
se_three <- sqrt(mse / ns["3"])
ci_three <- means["3"] + c(-1, 1) * tcrit * se_three

results <- data.frame(
  pushes = c(1, 3),
  mean = means
[c("1", "3")],
lower = c(ci_one[1], ci_three[1]),
upper = c(ci_one[2], ci_three[2])
)

# part d: Compute all pairwise comparisons + 95% CI (Tukey)
# ------------------------------------------------------------------------------
fit <- aov(waiting_time ~ pushes, data = push_dat)
TukeyHSD(fit)

# part e: Estimate contrast + 95% CI
# i: contrast the effect of no pushes vs. pushing the button once or more
# ------------------------------------------------------------------------------
c_hat <- means["0"] - (means["1"] + means["2"] + means["3"]) / 3

# calculate se(\hat{C})
cvec <- c(1, -1/3, -1/3, -1/3)
names(cvec) <- c("0", "1", "2", "3")

se <- sqrt(mse * sum(cvec^2 / ns[names(cvec)]))

tcrit <- qt(0.975, df)
ci <- c_hat + c(-1, 1) * tcrit * se

# part f: Test for linear trend in number of pushes
# ------------------------------------------------------------------------------
# linear trend contrast for a decreasing trend
cvec <- c("0" = 3, "1" = 1, "2" = -1, "3" = -3)
c_hat <- sum(cvec * means)
se <- sqrt(mse * sum(cvec^2 / ns[names(cvec)]))

ci <- c_hat + c(-1, 1) * tcrit * se

# part g: diagnostics
# ------------------------------------------------------------------------------
# check noramlity of error using qqplot of the residuals
plot(fit, which = 2)

# check zero expectation using plot of residuals vs. fitted values
plot(fit, which = 1)

# checking equality of variance using boxplot of residuals
boxplot(residuals(fit) ~ push_dat$pushes,
  xlab = "number of pushes",
  ylab = "residuals",
  main = "Residuals by Number of Pushes")
