lowbwt <- read.table("data/lowbwt.txt", header = T)

y <- lowbwt$headcirc # response vector
X <- cbind(rep(1, 100), lowbwt$gestage, lowbwt$toxemia) # design matrix
betahat <- solve(t(X)%*%X)%*%t(X)%*%y
betahat

r <- y - X%*%betahat # residual vector
sigma2hat <-  sum(t(r)%*%(r))/(100 - 3) # estimate for random error variance
sigma2hat

# Fit a multiple linear model
fit1 <- lm(headcirc ~ gestage + toxemia, data = lowbwt)
summary(fit1)

library(car)
workdat <- Prestige

# Create indicators
workdat$D1 <- ifelse(workdat$type == "wc", 1, 0)
workdat$D2 <- ifelse(workdat$type == "prof", 1, 0)
lm(prestige ~ D1 + D2, data = workdat) # manaul definition of dummy var.
lm(prestige ~ factor(type), data = workdat) # automatic definition of dummy var.

# fit a multiple linear regression with education and type
fit2 <- lm(prestige ~ education + factor(type), data = workdat)
summary(fit2)

# F-Test for the Overall Effect
# full model with education + type
fit_F <- lm(prestige ~ education + factor(type), dat = workdat)
# reduced model with education
fit_R <- lm(prestige ~ education, data = workdat)

# compare modles using ANOVA (F-test)
anova(fit_R, fit_F)

# F-Test for education*type Interaction
# full model with education, type, and education * type
fit_F <- lm(prestige ~ education + factor(type) + education*factor(type), data = workdat)

# reduced model with education and type
fit_R <- lm(prestige ~ education + factor(type), data = workdat)
anova(fit_R, fit_F)