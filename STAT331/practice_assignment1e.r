spring <- read.table("spring.txt", header = TRUE)

num <- sum(spring$Force * spring$Length)
denom <- sum(spring$Force^2)
beta_hat <- num/denom

pdf("spring_plot.pdf", width = 7, height = 5)
plot(spring$Force, spring$Length,
    main = "Scatterplot of spring force vs. length",
    xlab = "Force (pounds)",
    ylab = "Length (inches)",
    pch = 19,
    ylim = c(0, 18),
    xlim = c(0, 18)
)

abline(a = 0, b = beta_hat, col = "red", lwd = 2)
dev.off()