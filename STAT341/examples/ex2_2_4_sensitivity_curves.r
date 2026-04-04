# STAT341 Explicit Attributes 
# Exercise 2.2.4: Plotting sensitivity curves
# Using the given population:
N <- 1000
set.seed(341)
y.pop <- runif(N)

# for the following attributes, plot the sensitivity curves over the 
# ranges [-1, 2] & [-1000, 1000] and comment on each plot

sc <- function(y_pop, y, attr) {
  N <- length(y_pop) + 1
  sapply(y, function(y_new) {
    N * (attr(c(y_pop, y_new)) - attr(y_pop))
  })
}

plot_sc <- function(y_pop, attr, nam) {
  y1 <- seq(-1, 2, length.out = 1000)
  y2 <- seq(-1000, 1000, length.out = 1000)

  par(mfrow = c(1, 2), oma = c(0, 0, 2, 0), mar = 2.5 * c(1, 1, 1, 0.1))
  plot(y1, sc(y_pop, y1, attr), type = "l", col = "steelblue",
    xlab = "y", ylab = "SC(y)", main = ""
  )
  plot(y2, sc(y_pop, y2, attr), type = "l", col = "steelblue",
    xlab = "y", ylab = "SC(y)", main = ""
  )
  mtext(paste("Senstivity curve for ", nam), outer = TRUE, cex = 0.75)
}

# a) standard deviation
sdn <- function(y_pop) {
  N <- length(y_pop)
  sqrt(var(y_pop) * (N - 1) / N)
}
plot_sc(y.pop, attr = sdn, nam = "Standard Deviation")

# b) IQR
iqr <- function(y_pop) {
  quantile(y_pop, probs = 0.75) - quantile(y_pop, probs = 0.25)
}
plot_sc(y.pop, attr = iqr, nam = "Interquartile Range")

# c) Pearson's second skewness coefficient (median skewness)
pearson_skew <- function(y_pop) {
  y_bar <- mean(y_pop)
  med <- quantile(y_pop, probs = 0.5)
  y_sd <- sd(y_pop)
  3 * (y_bar - med) / y_sd
}
plot_sc(y.pop, attr = pearson_skew, nam = "Median Skewness")

# 2.2.5 Geometric Mean
# returns.txt: monthly returns of an investment over a period of 20 years.
# Plot the influence for each unit for the geometric mean along with the
# histogram of the data (use FD rule).
returns <- read.table("data/returns.txt", header = FALSE)
returns <- returns [, 1]

N <- length(returns)
inf <- (prod(returns))^(1 / N) - (prod(returns) / returns)^(1/(N - 1))
plot(inf, main= "Influence (Geometric Average)",
    ylab = "Influence")

hist(returns, main = "Monthly Returns on Investment",
  xlab = "Return Value", breaks = "FD")

# We see that there are two influential units relative to the others
# (influence below - 0.003)
influential_i <- which(inf < -0.003)
returns[influential_i]
sort(returns)[1:5]
# we see that these two influential observations are the smallest values of
# reutrns, which is consistent with the trend observed in the sensitivity curve
# (i.e. the closer the variate is to 0, the more influential it is)
geo_mean <- function(x) {
  (prod(x))^(1 / length(x))
}
plot_sc(returns, attr = geo_mean, "Geometric Mean")