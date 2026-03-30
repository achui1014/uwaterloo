# Horvitz-Thompson Toy Example (3.2.7.1)
# Recall the population P consisting of N = 5 units
set.seed(341)
pop5 <- round(rnorm(5), 2)
pop5 <- sort(pop5)
pop5

# take samples of size n = 2
sam2 <- combn(5, 2)
colnames(sam2) <- paste("S", 1:10, sep = " ")
sam2

# We will explore the HT estimation of the population mean using samples of
# size n = 2. The sample averages for all the samples are:
sam_avg <- apply(sam2, MARGIN = 2, FUN = function(s) {
    mean(pop5[s])
})
round(sam_avg, 3)

# there are two different sampling designs
# D1: each sample is selected with equal probability p(S) = 1/10
# D2: different samples have different probabilities
d1 <- rep(1/10, 10)
d2 <- 2 * (abs(apply(sam2, 2, diff)) - 1)
d2 <- d2 / sum(d2)
designs <- rbind(d1, d2)
colnames(designs) <- paste("S", 1:10, sep = " ")
round(designs, 2)

# sampling bias, variance, and MSE:
exp1 <- sum(sam_avg * d1)
exp2 <- sum(sam_avg * d2)

sam_bias <- c(exp1, exp2) - mean(pop5)
sam_var <- c(sum((sam_avg - exp1)^2 * d1), sum((sam_avg - exp2)^2 * d2))

designs_mse <- rbind(sam_bias, sam_var, MSE = sam_var + sam_bias^2)
colnames(designs_mse) <- c("d1", "d2")
round(designs_mse, 5)

# for HT calculations,we focus on design d2
# we require the marginal inclusion probabilities for each unit in the
# population and the joint inclusion probabilites for each possible pair

in_sample <- function(sam, N){
    insam <- numeric(N)
    insam[sam] <- 1
    insam
}
pop5sam2_units <- combn(5, 2, in_sample, N = 5)
rownames(pop5sam2_units) <- paste("unit", 1:5, sep = " ")
colnames(pop5sam2_units) <- paste("S", 1:10, sep = " ")
pop5sam2_units

# now for each unit, add up p(S) for each sample it appears in to yield pi_k
weighted_sum <- function(x, w) {
    sum(x * w)
}
pi2 <- apply(pop5sam2_units, 1, weighted_sum, w = d2)
pi2

# determine joint probabilities for d2
in_sample2 <- function(sam, N) {
    insam <- numeric(N)
    insam[sam] <- 1
    insam <- outer(insam, insam)
    insam
}

joint_sam_incl <- combn(5, 2, FUN = in_sample2, N = 5)
dimnames(joint_sam_incl) <- list(
    paste("unit", 1:5, sep = " "),
    paste("unit", 1:5, sep = " "),
    paste("S", 1:10, sep = " ")
)
joint_sam_incl[, , 1:3]

# now for each pair, add up p(S) for each sample they appear together to 
# yield joint inclusion probability
pij_2 <- apply(joint_sam_incl, c(1, 2), weighted_sum, w = d2)
pij_2

# use HT estimator
sam_HT <- apply(sam2, 2, function(s, x, wt) {
    sum(x[s] / wt[s])
}, x = pop5, wt = pi2) / 5
sam_HT

# sampling bias
sum((sam_HT - mean(pop5)) * d2)

# sampling variance
sum((sam_HT - sum(sam_HT * d2))^2 * d2)