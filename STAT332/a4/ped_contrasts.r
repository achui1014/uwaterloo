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
# a) Give a contrast to compare the effect of no pushes vs. pushing the button
# once or more
fit <- aov(waiting_time ~ pushes, data = push_dat)
means <- tapply(push_dat$waiting_time, push_dat$pushes, mean)

# contrast coefficients for:
# mu0 - (mu1 + mu2 + mu3)/3
cvec <- c("0" = 1, "1" = -1/3, "2" = -1/3, "3" = -1/3)
Chat <- sum(cvec * means[names(cvec)])

anova_tab <- summary(fit)[[1]]
MSE <- anova_tab["Residuals", "Mean Sq"]
n <- table(push_dat$pushes)
SE_Chat <- sqrt(MSE * sum(cvec^2 / n[names(cvec)]))
df <- df.residual(fit)

t <- qt(0.975, df)
CI <- c(Chat + c(-1, 1) * t * SE_Chat)

# b) Multiply the c_i's by 3 and compute the corresponding 95% CI
cvec_b <- cvec * 3
Chat_b <- sum(cvec_b * means[names(cvec_b)])
anova_tab <- summary(fit)[[1]]
MSE <- anova_tab["Residuals", "Mean Sq"]

SE_b <- sqrt(MSE * sum(cvec_b^2 / n[names(cvec_b)]))
CI_b <- c(Chat_b + c(-1, 1) * t * SE_b)

# part d: T-test for part a) and b)
# i) t-test for part a)
tobs_i <- Chat / SE_Chat
# ii) t-test for part b)
tobs_ii <- Chat_b / SE_b
