# Ch 3: Samples
# Example 3.0.1.1
agpop <- read.csv("data/agpop_data.csv", header = TRUE)
set.seed(341)
s <- sample(length(agpop$farms87), 100)

# Calculate the differences between attributes (population vs. sample)
c(
    mean(agpop$farms87[s]) - mean(agpop$farms87),
    median(agpop$farms87[s]) - median(agpop$farms87),
    sd(agpop$farms87[s]) - sd(agpop$farms87),
    IQR(agpop$farms87[s]) - IQR(agpop$farms87)
)

# Difference between histograms
par(mfrow = c(1, 2))
hist(agpop$farms87[s], breaks = 'FD', col = adjustcolor("grey", alpha = 0.5),
    main = "Number of Farms per County \n in 1987 - Sample",
    xlab = "Number of Farms", prob = TRUE, xlim = c(0, 6000))

hist(agpop$farms87, breaks = 'FD', col = adjustcolor("grey", alpha = 0.5),
    main = "Number of Farms per County \n in 1987 - Population",
    xlab = "Number of Farms", prob = TRUE, xlim = c(0, 6000))

# Example 3.1.1 Shark Data
sharks <- read.csv("data/sharks.csv", header = TRUE)
knitr::kable(head(sharks))

# for N = 65, generating all possible samples of size n = 5 can be
# computationally prohibitive
# * to reduce the computation, we focus on a sub-population of these encounters,
#   just those which occurred in Australian waters

# Units in the large population of all encounters
popSharks <- rownames(sharks)
popSharksAustralia <- popSharks[sharks$Australia == 1] # units in the sub-population

# generate all samples (n = 5) of the Australia Shark Data
samples <- combn(popSharksAustralia, 5)
M <- ncol(samples) # number of all possible samples
# this table shows which units are to be included in the first 5 and last samples
knitr::kable(data.frame(
    first = samples[, 1], second = samples[, 2],
    third = samples[, 3], fourth = samples[, 4],
    fifth = samples[, 5], last = samples[, M]
))

# calculate any attribute for any attribute
# (use apply fnction to apply FUN over its columns i.e. its second dimension)
# each column provides the row indices for that sample in the original pop.
avesSamp <- apply(samples,
    MARGIN = 2, # apply FUN over columns of the matrix
    FUN = function(s) {
        mean(sharks[s, "Length"])
    }
)

avesSamp[c(1:5, M)] # avg in the first 5 and last samples
