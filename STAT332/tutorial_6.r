# Tutorial 6: ANOVA Tables

tmp <- c("Ctrl1", "Ctrl2", "Labs", "Tutorials", "Self")
teaching_dat <- data.frame(
    test_score = c(17, 21, 28, 19, 21,
                14, 23, 39, 28, 14,
                24, 13, 29, 26, 13,
                20, 19, 24, 26, 19,
                24, 13, 27, 19, 15,
                23, 19, 30, 24, 15,
                16, 20, 28, 24, 10,
                15, 21, 28, 23, 18,
                24, 16, 23, 22, 20),
    method = rep(tmp, 9)
)
head(teaching_dat)

# part a and b: ANOVA table and testing whether all methods are the same
# -------------------------------------------------------------------------

# let's first look at boxplots
boxplot(test_score ~ method, data = teaching_dat)

# from the above boxplots, it seems as though the teaching methods yield
# different results; in particular, labs seems to be the superior method.

teaching_aov <- aov(test_score ~ method, data = teaching_dat)
summary(teaching_aov)

# we thus reject H_0 at the 5% level, and not all teaching methods yield the 
# same results

# Since ANOVA is a special case of linear regression, we can use the lm function
# in combination witht he anova function, and get the same results as above
anova(lm(test_score ~ method, data = teaching_dat))

# part c: Compute mean score for each section and give 95% CI
# -------------------------------------------------------------------------
tmp <- model.tables(teaching_aov, type = "means")

teaching_tab <- data.frame(names(tmp$tables[[2]]), c(tmp$tables[[2]]), tmp$n[[1]])
colnames(teaching_tab) <- c(names(tmp$tables[2]), "means", "ni")
rownames(teaching_tab) <- NULL

teaching_tab$var <- sum(teaching_aov$res^2) /
                 teaching_aov$df.residual * 1/teaching_tab$ni

teaching_tab$sd <- sqrt(teaching_tab$var)

teaching_tab$lower95 <- teaching_tab$means -
                     qt(0.975, teaching_aov$df.residual) * teaching_tab$sd

teaching_tab$upper95 <- teaching_tab$means +
                     qt(0.975, teaching_aov$df.residual) * teaching_tab$sd

teaching_tab