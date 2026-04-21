# Predictive Model and Cross Validation
# =============================================================================
# In this example, we show how to perform k-fold cross validation to select
# the best model based on the lowest Mean Squared Prediction Error (MSPE)
# * For all possible subsets/models, estimate MSPE via CV
# * Select the model with the lowest estimated MSPE
# =============================================================================
library(ISLR)
library(leaps)
library(glmnet)

data(Hitters)
Hitters <- na.omit(Hitters)
# response: Salary (in $1000s)
# 19 candidate predictors

# Cross-Validation For Each Model Size (Search within size)
n <- nrow(Hitters)
k <- 10 # 10-fold cv

# Randomly assign observations to each of the k folds
set.seed(123)
folds <- sample(1:k, n, replace = TRUE)
cv.errors <- matrix(NA, k, 19)

for(j in 1:k) {
    # Training set: all rows not in fold j
    train <- Hitters[folds != j, ]
    # Test set: all rows in fold j
    test <- Hitters[folds == j, ]

    # Fit best subset selection on training set
    regfit <- regsubsets(Salary ~ ., data = train, nvmax = 19)

    # Predict on the test set
    for(i in 1:19) {
        # coefficients of best i-predictor model
        coefi <- coef(regfit, id = i)
        
        # build model matrix from test data
        x.test <- model.matrix(Salary ~ ., data = test)

        # predict for obs in the test fold
        pred <- x.test[, names(coefi)] %*% coefi

        # compute test MSE for each model size
        cv.errors[j, i] <- mean((test$Salary - pred)^2)
    }
}

# Average squared prediction error across folds
mean.cv.errors <- apply(cv.errors, 2, mean)
# Plot CV prediction error vs. model size
plot(1:19, mean.cv.errors, type = "b",
    xlab = "Number of Predictors", ylab = "CV MSPE")

# Size of the best predictive model
best.size <- which.min(mean.cv.errors)
best.size

regfit.best <- regsubsets(Salary ~ ., data = Hitters, nvmax = best.size)
names(coef(regfit.best, id = best.size))