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
