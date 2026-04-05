# Main GD Function
gradient_descent <- function(theta = 0,
    gradientFn, rhoFn, linesearchFn, testconvFn,
    maxiterations = 100,
    tolerance = 1E-6, relative = FALSE,
    lambdaStepsize = 0.01, lambdaMax = 0.5) {
    
    converged <- FALSE
    i <- 0
    while(!converged && i <= maxiterations) {
        g <- gradientFn(theta)
        glength <- sqrt(sum(g^2))
        d <- g / glength
        lambda <- linesearchFn(theta, rhoFn, d,
            lambdaStepsize = lambdaStepsize, lambdaMax = lambdaMax
        )
        theta_new <- theta - lambda * d
        converged <- testconvFn(theta_new, theta,
            tolerance = tolerance,
            relative = relative
        )
        theta <- theta_new
        i <- i + 1
    }
    list(theta = theta, converged = converged,
        iterations = i, fnValue = rhoFn(theta))
}

# Necessary Helper Functions for GD
linesearchFn <- function(theta, rhoFn, d,
    lambdaStepsize = 0.01, lambdaMax = 1) {
    lambdas <- seq(0, lambdaMax, by = lambdaStepsize)
    rho_vals <- sapply(lambdas, function(lambda) {
        rhoFn(theta - lambda * d)
    })
    lambdas[which.min(rho_vals)]
}

testconvFn <- function(theta_new, theta_old,
    tolerance = 1E-10, relative = FALSE) {
    theta_diff <- sum(abs(theta_new - theta_old))
    theta_diff <- if (relative) {
        theta_diff / sum(abs(theta_old))
    } else {
        theta_diff
    }
    theta_diff < tolerance
}

# Example Usage: Simple Linear Regression using Factory Functions
createLSrho <- function(x, y) {
    xbar <- mean(x)
    function(theta) {
        alpha <- theta[1]
        beta <- theta[2]
        sum((y - alpha - beta * (x - xbar))^2)
    }
}

createLSGradient <- function(x, y) {
    xbar <- mean(x)
    function(theta) {
        alpha <- theta[1]
        beta <- theta[2]
        -2 * c(
            sum(y - alpha - beta * (x - xbar)),
            sum((y - alpha - beta * (x - xbar)) * (x - xbar))
        )
    }
}