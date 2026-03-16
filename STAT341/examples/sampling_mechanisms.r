# =========== STAT341: Implementation of Sampling Mechanisms ===================
# population: P = {u_1, ..., u_N}
# we wish to draw a sample S = {u_1, ..., u_N} from P
#
# A sampling mechanism specifies the probabilities p(u), p(u|k, s_k - 1)

set.seed(543270)

# Define P
N <- 100
n <- 10
# randomly sample 100 integers from 0 - 1000, wor
P <- sample.int(1000, N)
head(P)

# 1. Simple random sampling without replacement ================================
# Create a vector to hold the sample
set.seed(43987)
samp <- numeric(n)
idx <- 1:N

# do n draws in sequence
for (k in 1:n) {
    # sample one index uniformly at random
    nextidx <- sample(x = idx, size = 1, replace = FALSE, prob = rep(1/(N - k + 1), length(idx)))
    # Add that population value to the sample
    samp[k] <- P[nextidx]
    # Remove that index from the population
    idx <- idx[-which(idx == nextidx)]
}
samp

# or simply
sample(P, N, replace = FALSE)

# 2. Simple random sampling with replacement ================================
set.seed(438801)
samp <- numeric(n)
idx <- 1:N

# Do n draws in sequence
for (k in 1:n) {
    # Sample one index uniformly at random
    nextidx <- sample(x = idx, size = 1, replace = TRUE, prob = rep(1/N, length(idx)))
    # Add that population value to the sample
    samp[k] <- P[nextidx]
    # Do NOT remove that index from the population
}
samp

# or simply
sample(P, N, replace = TRUE)

# 2. Basu Weird Hybrid Sampling with replacement =============================
# de-duplicate SRSWR
unique(sample(P, N, replace = TRUE))

# OR
set.seed(438801)
samp <- numeric(n)
samp_idx <- numeric(n)
idx <- 1:N

# Do n draws in sequence
for (k in 1:n) {
    # Sample one index uniformly at random
    nextidx <- sample(x = idx, size = 1, replace = TRUE, prob = rep(1/N, length(idx)))
    # if already marked, skip
    if (nextidx %in% samp_idx) {next}
    # otherwise...
    # Add that population value to the sample and idx to samp_idx
    samp_idx[k] <- nextidx
    samp[k] <- P[nextidx]
    # Do NOT remove that index from the population
}
samp