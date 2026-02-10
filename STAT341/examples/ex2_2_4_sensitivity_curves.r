# STAT341 Explicit Attributes 
# Exercise 2.2.4: Plotting sensitivity curves
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

# e) The population mid-hinge
p_mid_hinge <- function(z) {(1/2) * (quantile(z, 0.75) + quantile(z, 0.25))}
plot.sc(y.pop, attr = p_mid_hinge, name = "Population Mid-Hinge")

# f) Pearson's moment coefficient of skewness
moment_coef_skewness <- function(z) {
    mean((z - mean(z))^3) / sdn(z)^3
}
plot.sc(y.pop, attr = moment_coef_skewness, name = "Pearson's Moment Coefficient of Skewness")

# Exercise 2.25: The Geometric Mean
# Plot the influence for each unit for the geometric mean along with the
# histogram of the data, and comment on the plots. Use Freedman Diaconis rule
# of bins.
returns <- read.table("data/returns.txt", header = FALSE)
returns <- returns[,1]

N <- length(returns)
delta <- (prod(returns))^(1/N) - (prod(returns)/returns)^(1/(N-1))

par(mfrow = c(1, 2))
plot(delta, main = "Influence (Geometric Average)", pch = 16)
hist(returns, col=adjustcolor("grey", alpha = 0.5),
    main = "Monthly Returns", xlab = "Return Value", breaks = 'FD')

# d) Plot the sensitivity curve of the geometric mean for this population over
# the ranges [0, 10] & [0.0001, 100] and interpret the plots
geo_mean <- function(x) {
    if (any(x < 0)) {
        return("All variate values must be positive")
    }
    return ( prod(x) ^ (1 / length(x)))
}

sc <- function(y.pop, y, attr, ...) {
    N <- length(y.pop) + 1
    Map(function(y) { N * (attr(c(y, y.pop), ...) - attr(y.pop, ...))}, y)
}

y1 <- seq(0, 10, length.out = 1000)
y2 <- seq(0.0001, 100, length.out = 1000)

par(mfrow = c(1, 2))
plot(y1, sc(returns, y1, attr = geo_mean), type = "l", lwd = 2,
    xlab = "y", ylab = "Sensitivity",
    main = "Sensitivity Curve for Geometric Mean")
abline(h = 0, v = 0, col = "red")

plot(y2, sc(returns, y2, attr = geo_mean), type = "l", lwd = 2,
    xlab = "y", ylab = "Sensitivity",
    main = "Sensitivity Curve for Geomtric Mean")
abline(h = 0, v = 0, col = "red")

# 2.2.14 Comparing Boxplots and Histograms
data(faithful)

# a.i) Summarize the data using the function summary and boxplot
summary(faithful)
boxplot(faithful)

# a.ii) Which variable seems to have a larger variation?
# The variable waiting has a larger variation

# a.iii) What is the connection between summary and boxplot for these data?
# The range and interquartiles which includes the median

# a.iv) Now using par(mfrow = c(1, 2)) to plot two separate boxplots but
# in the same figure. If we ignore th escale which variable seems to have a 
# larger variation?
par(mfrow = c(1, 2))
boxplot(faithful$eruptions, main = "Eruptions")
boxplot(faithful$waiting, main = "Waiting")
# The eruptions variable seems to have a larger variation

# a.v) Calculate the standard deviation and coefficient of variation for each
# variable and comment on the variation with respect to the two measures.
sdn <- function(x) {
    N <- length(x)
    sqrt( var(x) * (N - 1)/N)
}
coef_var <- function(z) { sdn(z) / mean(z)}
