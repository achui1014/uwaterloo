# Graphical Attributes
agpop <- read.csv("data/agpop_data.csv", header = TRUE)
agpop[agpop == -99] <- NA

hist(agpop$farms87,
    col = "grey",
    main = "Number of Farms per County in 1987",
    xlab = "Number of Farms",
    breaks = 100,
    # breaks: number of bins (approximately)
    prob = TRUE 
    # prob = TRUE -> calculates proportion of observations
    # prob = FALSE -> calculates number of observations (defualt)
)

x <- agpop$farms87
rx <- range(x)

length_out <- c(4, 5, 16)
par(mfrow = c(1, 3), mar = 2.5 * c(1, 1, 1, 0.1))
# bins with equal size (equal in width)
for (i in 1:3) {
    hist(x, breaks = seq(rx[1], rx[2], length.out = length_out[i]), 
        prob = TRUE, main = paste(length_out[i], " bins"),
        col = "lightblue"
    )
}

# bins with equal number of elements with varying size (approx. equal in area)
for (i in 1:3) {
    hist(x, breaks = quantile(x, p = seq(0, 1, length.out = length_out[i])),
    prob = TRUE, main = paste(length_out[i], "bins"),
    col = "lightpink"
    )
}

# Rules for Number of Bins
hist(x, prob = TRUE, xlab = "", main = "Sturges")
hist(x, breaks = "FD", prob = TRUE, xlab = "", main = "Freedman-Diaconis")
hist(x, breaks = "Scott", prob = TRUE, xlab = "", main = "Scott")

# Scatterplots
# dealing with duplicate values in integer-valued variates
# Jitter
feh <- read.csv("data/feh.csv", header = TRUE)
par(mfrow = c(1, 2))
plot(jitter(feh$RES, factor = 1.5), jitter(feh$DEF, factor = 1.5),
    main = "Raw Values + Jitter", pch = 19, cex = 0.5,
    col = adjustcolor("black", alpha = 0.3),
    xlab = "Resistance", ylab = "Defense"
)
plot(feh$RES, feh$DEF,
    main = "Raw Values",
    pch = 19, cex = 0.5,
    col = adjustcolor("black", alpha = 0.3),
    xlab = "Resistance", ylab = "Defense"
)

# Scatter-plot of y_u vs. rank
y <- agpop$acres87[agpop$region == "NE"]
y <- na.omit(y)
yrank <- rank(y, ties.method = "first")
par(mfrow = c(1, 1))
plot(yrank, y, pch = 19, col = "#c7df70",
    xlab = "County rank by acreage",
    ylab = "Farming acres in 1987",
    main = "Counties in the North East USA"
)

# scatter-plot of proportion vs. y_u
N <- length(y)
p <- yrank / N
plot(p, y, pch = 19, col = "#c7df70", xlim = c(0, 1),
    xlab = "Proportion (p)",
    ylab = "Farming acres in 1987",
    main = "Counties in the North East USA"
)
