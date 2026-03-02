feh <- read.csv("data/feh.csv", header = TRUE)

# Plot the linear model of defense vs. resistance
# assess whether the regression line is a good fit for this data
plot(feh$RES, feh$DEF, main = "Linear Model of Defense vs. Resistance",
    pch = 19, cex= 1.5, col=adjustcolor("black", alpha = 0.3),
    xlab = "Reistance", ylab = "Defense")

fit <- lm(DEF ~ RES, data = feh)
abline(fit, col = "red", lwd = 2)

# Calculate influence
N = nrow(feh)
delta <- matrix(0, nrow = N, ncol = 2)
for (i in 1:N) {
    ## feh[-i] removes the ith row from a vector
    fit.no.i <- lm(DEF ~ RES, data = feh[-i,])
    delta[i,] <- abs(fit$coef - fit.no.i$coef) # calculates the absolute influence
}

# Influence on Intercept (alpha)
par(mfrow=c(1,2))
hist(delta[,1], breaks="FD", main=bquote("Influence on" ~ alpha),
    xlab=bquote(Delta[alpha]), col = adjustcolor("grey", 0.6))

plot(delta[,1], ylab = bquote(Delta[alpha]), 
    main = bquote("Influence on" ~ alpha), 
    pch = 19, col = adjustcolor("grey", 0.6))

# Influence on Slope (beta)
par(mfrow=c(1,2))
hist(delta[,2], breaks="FD", main=bquote("Influence on" ~ beta),
    xlab=bquote(Delta[beta]), col=adjustcolor("grey", 0.6))

plot(delta[,2], ylab=bquote(Delta[beta]),
    main = bquote("influence on" ~ beta),
    pch = 19, col = adjustcolor("grey", 0.6))

# Influence on the regression line as a whole (theta)
par(mfrow = c(1, 2))
delta2 <- apply(X = delta, MARGIN = 1, FUN = function(z) {sqrt(sum(z^2))})
hist(delta2, breaks = "FD", main = bquote("Influence on" ~ theta),
    xlab = bquote(Delta), col = adjustcolor("grey", 0.6))

plot(delta2, main = bquote("Influence on" ~ theta),
    ylabb = bquote(Delta), pch = 19, col=adjustcolor("grey", 0.6))

# The two units with the largest influence
feh[delta2 > 0.5,]
# Highlight these units in the plot
col.nam <- c(adjustcolor("black", alpha = 0.3), 
    adjustcolor("firebrick", alpha = 0.6))

plot(feh$RES, feh$DEF, main = "",
    pch = 19, cex = 1.5,
    col = col.nam[(delta2 > 0.5) + 1],
    xlab = "Resistance", ylab = "Defense")
abline(fit, col =2, lwd = 2)

# ======= Dealing with Influential Units in Linear Regression ==================
# METHOD 1: ===== Removing Influential Units ===================================
library(robustbase)
head(Animals2, n = 5)
summary(Animals2)
plot(Animals2, main = "Brain vs. Body Weight",
    xlab = "body weight (kg)", ylab = "brain weight (g)",
    pch = 16, col = adjustcolor("black", alpha.f = 0.3))
# since both variates are very skewed, we will use a power transformation (log)
powerfun <- function(y, alpha) {
    if (sum(y <= 0) > 0) stop("y must be postive")
    if (alpha == 0)
        log(y)
    else if (alpha > 0) {
        y^ alpha
    } else -y^alpha
}
par(mfrow=c(3,3), mar=2.5*c(1,1,1,0.1))
a <- rep(c(0, 1/2, 1), each = 3)
b <- rep(c(0, 1/2, 1), times = 3)

for (i in 1:9) {
    plot(powerfun(Animals2$body, a[i]), powerfun(Animals2$brain, b[i]),
    pch = 19, cex = 0.5, col = adjustcolor("black", alpha = 0.3), xlab = "", ylab = "",
main = bquote(alpha[x] == .(a[i]) ~ "," ~ alpha[y] == .(b[i])))
}
# then choose the preferred transformation
plot(x = log(Animals2$body), y = log(Animals2$brain),
    main = "Brain vs. Body Weight (log-transform)",
    xlab = "log(body weight)", ylab = "log(brain weight)",
    pch = 16, col = adjustcolor("black", alpha.f = 0.3))
abline(lm(log(Animals2$brain) ~ log(Animals2$body)), col = "red")

# Analyze the residuals
mod <- lm(log(Animals2$brain) ~ log(Animals2$body))
plot(residuals(mod), col = adjustcolor("black", alpha = 0.3),
    ylab = "Reisduals", xlab = "Index", pch = 19) 

# the plot above confirms that there are three outliers
# remove them:
plot(x = log(Animals2$body), y = log(Animals2$brain),
    main = "Brain vs. Body Weight (log-transform)",
    xlab = "log(body weight)", ylab = "log(brain weight)",
    pch = 16, col = adjustcolor("black", alpha.f = 0.3))
abline(mod, col = 2)

indx <- which(log(Animals2$body) > 9)
W <- rep(1, 65)
W[indx] <- 0
abline(lm(log(Animals2$brain) ~ log(Animals2$body), weights = W), col = 4)

# what are these units?
Animals2[indx,]
# These three data points are dinosaurs!

# ==============================================================================
# METHOD 2: ==== Weighted Linear Regression ====================================
res <- residuals(mod)
outlier_sd <- sqrt(sum(res[indx]^2)/length(indx))
remaining_sd <- sqrt(sum(res[indx]^2)/(length(res[-indx])))

c(outlier_sd, remaining_sd)
# Note that the residual standard deviations are around 0 for both the outliers
# and the reamining points

# The relative variation is
remaining_sd/outlier_sd
# since the dinosaurs have larger residual variation, we should give them
# less weight:
# wu = 1 for mammals; 0.2349 for dinosaurs

plot(x = log(Animals2$body), y = log(Animals2$brain),
     main = "",
     xlab = "log(body weight)", ylab = "log(brain weight)",
     pch = 16, col = adjustcolor("black", alpha.f = 0.3))

abline(mod, col = "red")

W <- rep(1, 65)
W[which(log(Animals2$body) > 9)] <- 0 # weight 1 for remainder, 0 for outliers
abline(lm(log(Animals2$brain) ~ log(Animals2$body), weights =  W), col = "blue")

wt <- rep(1, 65)
wt[obs] <- remaining_sd / outlier_sd
abline(lm(log(Animals2$brain) ~ log(Animals2$body), weights = wt),col ="purple")

legend("bottomright",
     legend = c("LS line", "LS line (no outliers", "WLS line"),
     col = c("red", "blue", "purple"),
     cex = 0.75, bty = "n", lty = 1)

# ==============================================================================
# METHOD 3: ========== Robust Regression =======================================
plot(x = log(Animals2$body), y = log(Animals2$brain),
     main = "",
     xlab = "log(body weight)", ylab = "log(brain weight)",
     pch = 16, col = adjustcolor("black", alpha.f = 0.3))

abline(mod, col = "red")

W <- rep(1, 65)
W[which(log(Animals2$body) > 9)] <- 0 # weight 1 for remainder, 0 for outliers
abline(lm(log(Animals2$brain) ~ log(Animals2$body), weights =  W), col = "blue")

wt <- rep(1, 65)
wt[obs] <- remaining_sd / outlier_sd
abline(lm(log(Animals2$brain) ~ log(Animals2$body), weights = wt),col ="purple")

library(MASS)
abline(rlm(log(Animals$brain) ~ log(Animals2$body),psi ="psi.huber"),col= "green")

library(L1pack)
abline(lad(log(Animals2$brain) ~ log(Animals2$body)), col = "orange")

legend("bottomright",
     legend = c("LS line", "LS line (no outliers", "WLS line", "Huber Line", "LAD line"),
     col = c("red", "blue", "purple", "green", "orange"),
     cex = 0.75, bty = "n", lty = 1)