frog_jumps <- data.frame(
    dimension = c(
        rep(10, 10),
        rep(15, 10),
        rep(20, 10)
    ),
    distance  = c(
        24, 20, 30, 36, 21, 23, 42, 19, 27, 26,
        26, 21, 27, 17, 22, 21, 18, 43, 30, 24,
        35, 22, 36, 44, 29, 36, 35, 30, 32, 27
    )
)
frog_jumps$dimension <- as.factor(frog_jumps$dimension)

# part a: ANOVA table
# ------------------------------------------------------------------------------
boxplot(distance ~ dimension, data = frog_jumps)
aov_tab <- anova(lm(distance ~ dimension, data = frog_jumps))
aov_tab

# part c: pairwise comparisons (Tukey's method)
# ------------------------------------------------------------------------------
fit <- aov(lm(distance ~ dimension, data = frog_jumps))
TukeyHSD(fit)

# part e: linearity contrast
# ------------------------------------------------------------------------------
means <- tapply(frog_jumps$distance, frog_jumps$dimension, mean)
ns <- tapply(frog_jumps$distance, frog_jumps$dimension, length)
mse <- aov_tab["Residuals", "Mean Sq"]
df <- aov_tab["Residuals", "Df"]

c_hat <- means["20"] - means["10"]

# calculate se(\hat{C})
cvec <- c(-1, 0, 1)
names(cvec) <- c("10", "15", "20")

se <- sqrt(mse * sum(cvec^2 / ns[names(cvec)]))

tcrit <- qt(0.975, df)
ci <- c_hat + c(-1, 1) * tcrit * se

# part f: diagnostics
# ------------------------------------------------------------------------------
png("qqplot.png", width = 500, height = 400)
# check noramlity of error using qqplot of the residuals
plot(fit, which = 2)
dev.off()

# check zero expectation using plot of residuals vs. fitted values
png("fitted_residuals.png", width = 500, height = 400)
plot(fit, which = 1)
dev.off()

# checking equality of variance using boxplot of residuals
png("boxplot.png", width = 500, height = 400)

boxplot(residuals(fit) ~ frog_jumps$dimension,
  xlab = "Dimension",
  ylab = "Residuals",
  main = "Residuals by Dimension")
dev.off()