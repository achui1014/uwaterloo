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