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

# part c: mean waiting times + 95% CI
# ------------------------------------------------------------------------------
wait_times_aov <- aov(waiting_time ~ pushes, data = push_dat)
tmp <- model.tables(wait_times_aov, type = "means")
pushes_tab <- data.frame(names(tmp$tables[[2]]), c(tmp$tables[[2]]), tmp$n[[1]])
colnames(pushes_tab) <- c(names(tmp$tables[2]), "means", "ni")
rownames(pushes_tab) <- NULL

pushes_tab$var <- sum(wait_times_aov$res^2) /
  wait_times_aov$df.residual * 1 / pushes_tab$ni
pushes_tab$sd <- sqrt(pushes_tab$var)

pushes_tab$lower95 <- pushes_tab$means -
  qt(0.975, wait_times_aov$df.residual) * pushes_tab$sd
pushes_tab$upper95 <- pushes_tab$means +
  qt(0.975, wait_times_aov$df.residual) * pushes_tab$sd

###
model <- aov(waiting_time ~ pushes, data = push_dat)
summary(model)

mse <- summary(model)[[1]]["Residuals", "Mean Sq"]
df <- summary(model)[[1]]["Residuals", "Df"]

means <- sapply(push_dat$waiting_time, mean)
