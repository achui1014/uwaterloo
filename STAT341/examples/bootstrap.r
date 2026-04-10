sharks <- read.csv("data/sharks.csv", header = TRUE)
pop_sharks <- rownames(sharks)

# the code below constructs an approximate sampling distribution for esitmates
# of the average shark length based on samples of size n = 6
# there are 65 choose 6 = 82,598,880 possible samples
sdn <- function(y_pop) {
    N <- length(y_pop)
    sqrt(var(y_pop) * (N - 1) / N)
}
M <- 10^4 # approximate sampling distribution is based on M samples
n <- 6
set.seed(341)
samples <- sapply(1:M,
    FUN = function(m) sample(pop_sharks, n, replace = TRUE))

# function of interest: averages
ave_pop <- mean(sharks[, "Length"])
ave_samp <- apply(samples, MARGIN = 2,
    FUN = function(s) {
        mean(sharks[s, "Length"])
    }
)
samp_error <- ave_samp - ave_pop

tmp_ave <- mean(ave_samp)
tmp_SD <- sdn(ave_samp)

sd_samp <- apply(samples, MARGIN = 2,
    FUN = function(s) {
        sd(sharks[s, "Length"])
    }
)

# Apply bootstrap method: draw sample S from P using SRSWR
get_sample <- function(pop, size, replace = FALSE) {
    N <- length(pop)
    pop[sample(1:N, size, replace = replace)]
}
S <- get_sample(1:65, n, replace = FALSE)
# draw bootstrap samples of size n = 6
# there are 6^6 = 45,656 such possible samples to select
# we choose B = 10,000 bootstrap samples
P_star <- S
B <- 10000
set.seed(341)
S_star <- sapply(1:B, FUN = function(b) {
    get_sample(P_star, n, replace = TRUE)
})

# compute avg shark length for each bootstrap sample
ave_bootsamp <- sapply(1:B, FUN = function(i) mean(sharks[S_star[,i], "Length"]))

par(mfrow = c(1, 2))
h_pop_ave <- hist(extendrange(c(ave_samp, ave_bootsamp)), breaks = 50,
    plot = FALSE)
hist(ave_bootsamp,
    xlim = range(ave_samp), breaks = h_pop_ave$breaks,
    freq = FALSE, col = "grey",
    main = "B = 10,000 Bootstrap Averages \n(n = 6)"
)
hist(ave_samp,
    xlim = range(ave_samp), breaks = h_pop_ave$breaks,
    freq = FALSE, col = "grey",
    main = "M = 10,000 Sample Averages \n(n = 6)"
)

# compare variability by constructing histograms for
# sample error vs. bootstrap sample error
ave_sam <- mean(sharks[S, "Length"])
range_avediff <- extendrange(c(
    ave_samp - ave_pop,
    ave_bootsamp - ave_sam
))
h_pop_ave_diff <- hist(range_avediff, breaks = 50, plot = FALSE)
hist(ave_bootsamp - ave_sam,
    xlim = range_avediff, breaks = h_pop_ave_diff$breaks,
    freq = FALSE, col = "pink",
    main = "B = 10,000 Bootstrap Sample Errors \n(n = 6)"
)
hist(ave_samp - ave_pop,
    xlim = range_avediff, breaks = h_pop_ave_diff$breaks,
    freq = FALSE, col = "pink",
    main = "M = 10,000 Sample Errors \n(n = 6)"
)

# e.g. inference about a population average
# note: when the attribute of interest is an average, we can apply the finite
# population correction
N <- dim(sharks)[1]
c(sdn(ave_bootsamp),
    sdn(sharks[S, "Length"]) / sqrt(n) * sqrt((N - n) / (N - 1)))
# if the sample is not a good representation of the populations, these two
# numbers might be quite different
