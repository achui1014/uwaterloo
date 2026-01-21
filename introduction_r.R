p = 3
# Perform an iterative task with a for loop
x = list()
set.seed(341)
for(i in 1:10^4){
  x[[i]] = matrix(rnorm(p^2), p, p)
}
print(x[1:2])

# Perform the same iterative task with an apply function
set.seed(341)
x = mapply( function(x) { matrix(rnorm(p^2), p, p) }, 1:10^4, SIMPLIFY = FALSE)
print(x[1:2])

# Perform an accumulative task with a for loop
z = matrix(0, p, p)
for(i in 1:10^4) {
  z = z + x[[i]]
}
print(z)

### Titatnic ###
data(Titanic) # load the data into R's global environment

require(graphics)
mosaicplot(Titanic, main = "Survival")
#Higher survival rates in children?
apply(Titanic, c(3,4), sum)

#Higher survival rates in females?
apply(Titanic, c(2,4), sum)

