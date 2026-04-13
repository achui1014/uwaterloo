# Ch9: Randomized Block Designs
# CEO Example:

tmp <- scan()
1
2
7
6
12
5
8
9
13
14
8
14
16
18
17

ceo.dat <- data.frame(premium=tmp, block=rep(1:5, 3),
             method=rep(c("utility", "worry", "comparison"), c(5,5,5)))
ceo.dat$block <- as.factor(ceo.dat$block)
ceo.dat$method <- as.factor(ceo.dat$method)

# Fitting the randomized block design/ANOVA model
ceo.aov <- aov(premium ~ method + block, data = ceo.dat)
summary(ceo.aov)

# Multiple Comparisons
TukeyHSD(ceo.aov, "method")

model.tables(ceo.aov, type = "means")

# Model Assumption Checking
# 1. Normality: QQ plot
plot(ceo.aov, which = 2)

# 2. Residual vs. Fitted Values
plot(ceo.aov, which = 1)

# Tukey Test for Additivity
library(asbio)
tukey.add.test(y = ceo.dat$premium, A = ceo.dat$method, B = ceo.dat$block)