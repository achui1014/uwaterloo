# STAT331: Ch4 Model Selection in Linear Regression
# Lasso Regression Example

library(ISLR)
data(Hitters)
Hitters <- na.omit(Hitters)
head(Hitters)
# this dataset studies the relationship between baseball player salaries
# and performance

# LASSO with 10-fold CV
library(glmnet)
x <- model.matrix(Salary ~ ., Hitters)[, -1]
y <- Hitters$Salary

set.seed(123)
cv.lasso <- cv.glmnet(x, y, alpha = 1, nfolds = 10)
plot(cv.lasso)

# Best lambda values
cv.lasso$lambda.min # lambda that minimizes CV error
cv.lasso$lambda.1se # more regularized (simpler model)

# Coefficients at selected lambda
coef_min <- as.matrix(coef(cv.lasso, s = "lambda.min"))
coef_1se <- as.matrix(coef(cv.lasso, s = "lambda.1se"))

coef_compare <- cbind(coef_min, coef_1se)
colnames(coef_compare) <- c("lambda.min", "lambda.1se")
coef_compare