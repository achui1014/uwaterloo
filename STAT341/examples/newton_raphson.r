# newton's method
newton <- function(theta = 0,
    psiFn, psi_primeFn,
    testconvFn = testconvFn,
    maxiterations = 100,
    tolerance = 1E-6,
    relative = FALSE) {

    converged <- FALSE
    i <- 0
    while(!converged && i <= maxiterations) {
        theta_new <- theta - psiFn(theta) / psi_primeFn(theta)
        converged <- testconvFn(theta_new, theta,
        tolerance = tolerance, relative = relative
        )
        theta <- theta_new
        i <- i + 1
    }
    list(theta = theta, converged = converged,
        iterations = i, fnValue = psiFn(theta))
}

# newton-raphson
newton_raphson <- function(theta,
    psiFn, psi_primeFn, dim,
    testconvFn = testconvFn,
    maxiterations = 100, tolerance = 1E-6, relative = FALSE) {
    
    converged <- FALSE
    i <- 0
    while(!converged && i <= maxiterations) {
        theta_new <- theta - solve(psiFn(theta), psi_primeFn(theta))
        converged <- testconvFn(theta_new, theta,
            tolerance = tolerance, relative = relative
        )
        theta <- theta_new
        i <- i + 1
    }
    list(theta = theta, converged = converged,
        iterations = i, fnValue = psiFn(theta))
}