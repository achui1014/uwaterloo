wine <- read.table("wine.txt", header = TRUE)

# Response vector
y <- wine$Quality
# Design matrix
x <- cbind(rep(1, 38), wine$Clarity, wine$Aroma, wine$Body, wine$Flavor, wine$Oakiness)
# Least squares estimates for regression coefficients
betahat <- solve(t(x)%*%x) %*% t(x) %*%y
betahat

# Fit a multiple linear model
fit <- lm(Quality ~ Clarity + Aroma + Body + Flavor + Oakiness, data = wine)
summary(fit)

qt(0.965, df = 38 - 5 - 1) # t-value used for hypothesis testing

reduced_fit <- lm(Quality ~ Aroma + Flavor + Oakiness, data = wine)
summary(reduced_fit)

newdat <- data.frame(Aroma = 6, Flavor = 7, Oakiness = 3)
predict.lm(reduced_fit, newdat, interval = "predict", level = 0.95)