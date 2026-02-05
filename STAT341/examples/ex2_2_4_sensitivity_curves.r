# STAT341 Explicity Attributes Exercise 2.2.4: Plotting sensitivity curves
# Using the given population:
N <- 1000
set.seed(341)
y.pop <- runif(N)

# for the following attributes, plot the sensitivity curves over the 
# ranges [-1, 2] & [-1000, 1000] and comment on each plot

# Setup
plot.sc <- function(y.pop, y, attr, name = "Standard Deviation") {
    N <- length(y.pop) + 1 # size of new population
    sc <- function(y, attr) {
        Map(function(y) {
            N * attr(c(y, y.pop)) - attr(y.pop)
        }, y)
    }

    y1 <- seq(-1, 2, length.out = 1000)
    y2 <- seq(-1000, 1000, length.out = 1000)

    par(mfrow = c(1, 2), oma = c(10, 0, 10, 0), mar = 2.5 * c(1, 1, 1, 0.1))
    plot(y1, sc(y1, attr), type="l", col = "steelblue",
        xlab = "y", ylab = "SC(y)", main = "")
    plot(y2, sc(y2, attr), type = "l", col = "steelblue",
        xlab = "y", ylab = "SC(y)", main = "")

    mtext(paste("Sensitivity curve for ", name), outer = TRUE, cex = 1.5)
}

# a) Standard deviation
sdn <- function(y.pop) {
    N <- length(y.pop)
    sqrt( var(y.pop) * (N - 1)/N)
}

plot.sc(y.pop, attr = sdn, name = "Standard Deviation")

# b) Interquartile range
plot.sc(y.pop, attr = IQR, name = "IQR")

# c) Pearson's second skewness coefificient (median skewness)
skew <- function(z) {3 * (mean(z) - median(z)) / sdn(z)}
plot.sc(y.pop, attr = skew, name = "Pearson's Skewness Coefficient")

# d) Coefficient of variation
coef_var <- function(z) { sdn(z) / mean(z)}
plot.sc(y.pop, attr = coef_var, name = "Coefficient of Variation")