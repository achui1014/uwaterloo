# Introduction
# Mapping: Apply a function to each element of a container
# (container examples: array, list, rows of a matrix, etc.)
# * maps elements of type A to elements of type B
p <- 3

# using a for loop
x <- list()
set.seed(341)
for (i in 1:10^4) {
  x[[i]] <- matrix(rnorm(p^2), p, p)
}
print(x[1:2])

# using the apply function
x <- mapply(function(x) {
  matrix(rnorm(p^2), p, p)
}, 1:10^4, SIMPLIFY = FALSE)
print(x[1:2])

# Reduce: Combine all elements of a container into a single unit
# using a for loop
z <- matrix(0, p, p)
for (i in 1:10^4) {
  z <- z + x[[i]]
}
print(z)

# using the apply function
z <- apply(simplify2array(x), MARGIN = c(1, 2), FUN = sum)
print(z)
