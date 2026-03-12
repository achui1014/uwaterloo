# General purpose implementation of Gradient Descent
gradient_descent <- function(theta = 0, rhoFn, gradientFn, lineSearchFn, testConvergenceFn,
                            maxIterations = 100,
                            tolerance = 1E-6, relative = FALSE,
                            lambdaStepsize = 0.01, lambdaMax = 0.5) {
  converged <- FALSE
  i <- 0
  while (!converged & i <= maxIterations) {
    g <- gradientFn(theta) # gradient
    glength <- sqrt(sum(g^2)) # gradient direction
    if (glength > 0) d <- g / glength

    lambda <- lineSearchFn(theta, rhoFn, d, lambdaStepsize = lambdaStepsize,
                           lambdaMax = lambdaMax)
    thetaNew <- theta - lambda * d # update iterate
    converged <- testConvergenceFn(thetaNew, theta, tolerance = tolerance,
                                   relative = relative)
    theta <- thetaNew
    i <- i + 1
  }
  # Return last value and whether converged or not
  list(theta = theta, converged = converged, iteration = i, fnValue = rhoFn(theta))
  }

# Line search using grid search
gridLineSearch <- function(theta, rhoFn, d, lambdaStepsize = 0.01, lambdaMax = 1) {
    # grid of lambda values to search
    lambdas <- seq(from = 0, by = lambdaStepsize, to = lambdaMax)

    # line search
    rhoVals <- sapply(lambdas, function(lambda) {rhoFn(theta - lambda * d)})

    # Return the lambda that gave the minimum
    lambdas[which.min(rhoVals)]
}

# testCovergence (relative or absolute)
testConvergence <- function(thetaNew, thetaOld, tolerance = 1E-10, relative = FALSE) {
    thetaDiff <- sum(abs(thetaNew - thetaOld))
    thetaDiff <- if (relative) tolerance * sum (abs(thetaOld)) else tolerance
}

# Ex 1: One-Dimensional Quadratic Function
# rho(theta) = 2theta^2 - 5theta + 3
rho <- function(theta) {
    2 * theta^2 - 5 * theta + 3
}
g <- function(theta) {4 * theta - 5}
gradient_descent(rhoFn = rho, gradientFn = g,
    lineSearchFn = gridLineSearch,
    testConvergenceFn = testConvergence)

# Example 2: Two-dimensional Rosenbrock Function
rho <- function(theta) {
  (1 - theta[1])^2 + 100 * (theta[2] - theta[1])^2
}

g <- function(theta) {
  c(
    400 * theta[1]^3 - 400 * theta[2] * theta[1] + 2 * theta[1] - 2,
    -200 * theta[1]^2 + 200 * theta[2]
  )
}
gradient_descent(rhoFn = rho, gradientFn = g,
                 lineSearchFn = gridLineSearch, lambdaStepsize = 0.001,
                 testConvergenceFn = testConvergence, maxIterations = 1000)

# Factory Function for rho and gradient
create_least_squares_rho <- function(x,y) {
  xbar <- mean(x) # local variable
  # return the following function
  function(theta) {
    alpha <- theta[1]
    beta <- theta[2]
    sum((y - alpha - beta * (x - xbar))^2)
  }
}

# get the rho function
rho <- create_least_squares_rho(waldo$X, waldo$Y)

# similarly for the gradient function
create_least_squares_grad <- function(x,y) {
  xbar <- mean(x) # local variable
  function(theta) {
    alpha <- theta[1]
    beta <- theta[2]
    -2 * c(sum(y - alpha - beta * (x - xbar)),
           sum((y - alpha - beta * (x - xbar)) * (x - xbar)))
  }
}
gradient <- create_least_squares_grad(waldo$X, waldo$Y)