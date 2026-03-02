# The vee function of population variates to obtain the least generalized-absolute deviations
# when this function is minimized for some value q in (0, 1), we get the qth quantile
vee <- function(z, q) {
    val <- q * z
    val[z < 0] <- (q - 1) * z [z < 0]
    return(val)
}

# ex. finding the 30th quantile by minimizing a sum of appropriate vee functions
set.seed(341)
y.pop <- rnorm(25, 10, 3)

# define the loss function (i.e. sum of vee functions) to be minimized
rho.fun <- function(theta, q.val) {
    return (sum(sapply(X = y.pop - theta, FUN = vee, q = q.val)))
}

# plot the function
theta <- seq(6, 10, length.out = 1000)
rho.vals <- numeric(length(theta))
for (i in 1: length(theta)) {
    rho.vals[i] <- rho.fun(theta[i], q.val = 0.3)
}

plot(theta, rho.vals, type = "l",
    xlab = bquote(theta), ylab = bquote(rho[]),
    main = "Loss Functino to be Minimized")

# Use a numerical minimizer to find the argument that minimizes this function
# method 1: nlminb
nlminb(start = 0.5, objective = rho.fun, q.val = 0.3)

# method 2: optimize
optimize(f = rho.fun, interval = range(y.pop), q.val = 0.3)

# method 3: optim
optim(par = 0.5, fn = rho.fun, q.val = 0.3)