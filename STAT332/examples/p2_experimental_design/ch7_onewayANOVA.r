# Chapter 7 - One-way ANOVA
# Compute ANOVA table and F test
# ex 7.5.1 Kenton Food Company

# Load Kenton Food Company data
Kenton.dat <- data.frame(scan(what=list(0,0,0)))

1  1  12
1  2  18
2  1  14
2  2  12
2  3  13
3  1  19
3  2  17
3  3  21 
4  1  24
4  2  30

names(Kenton.dat) <- c("packDesign", "store", "cases")
# Treat packDesign and store as factors not continuous variables!!!!!!!
Kenton.dat$packDesign <- as.factor(Kenton.dat$packDesign)
Kenton.dat$store <- as.factor(Kenton.dat$store)

head(Kenton.dat)
summary(Kenton.dat)
boxplot(cases ~ packDesign, data = Kenton.dat, xlab = "Package design",
    ylab = "Nb. of cases sold", mean = T)

# Create ANOVA table and F test
kenton.aov <- aov(cases ~ packDesign, data = Kenton.dat)
summary(kenton.aov)

# Estimate factor level means
model.tables(kenton.aov, type = "means")
tmp <- model.tables(kenton.aov, type = "means")
kenton.tab <- data.frame(names(tmp$tables[[2]]), c(tmp$tables[[2]]))
colnames(kenton.tab) <- c(names(tmp$tables[2]), "means")

# estimate CI for factor level mean
kenton.tab$var <- sum(kenton.aov$res^2) / kenton.aov$df.residual * 1 / c(tmp$n[[1]])
kenton.tab$sd <- sqrt(kenton.tab$var)
kenton.tab$lower95 <- kenton.tab$means -
    qt(0.975, kenton.aov$df.residual) * kenton.tab$sd
kenton.tab$upper95 <- kenton.tab$means +
    qt(0.975, kenton.aov$df.residual) * kenton.tab$sd
kenton.tab

# Multiple comparisons
TukeyHSD(kenton.aov)
plot(TukeyHSD(kenton.aov))

# Check model assumptions
# 1. Normality
# To check this assumption, we draw a QQ plot of the residuals. If the assumption is met,
# then the points should fall on a straight line
plot(kenton.aov, which = 2)

# 2. Zero Expectation
# To check this assumption, we plot the residuals vs. fitted values. If the assumption is
# met, then about half of the residuals should be above 0 and the other half below.
plot(kenton.aov, which = 1)

# 3. Equality of Error Variance
# To check this assumption, we use both a residuals vs. fitted values plot as well as a 
# side-by-side boxplot of residuals. If the assumption is met, these residual plots should
# show the same extent of scatter of residuals around 0 for each factor level.
kenton.res <- data.frame(res = residuals(kenton.aov),
    packDesign = Kenton.dat$packDesign)
boxplot(res ~ packDesign, data = kenton.res, xlab = "Packagae Design",
    ylab = "Residuals", mean = TRUE)

tmp <- split(kenton.res$res, kenton.res$packDesign)
tmp <- sapply(tmp, var)
tmp2 <- c(model.tables(kenton.aov, type="means")$n[[1]])

tmp * tmp2/(tmp2-1)

# Outliers
kenton.res$packDesign <-  as.numeric(kenton.res$packDesign)
plot(kenton.res$packDesign, kenton.res$res, type="n", xlab="Pack Design",
     ylab="Residuals", xaxp=c(1,4,3), ylim=c(-6.5, 6.5))
kenton.res <- split(kenton.res$res, kenton.res$packDesign)
points(rep(1, length(kenton.res[[1]])), kenton.res[[1]], col=1, pch=20, cex=1.25)
points(rep(2, length(kenton.res[[2]])), kenton.res[[2]], col=4, pch=17)
points(rep(3, length(kenton.res[[3]])), kenton.res[[3]], col=4, pch=17)
points(rep(4, length(kenton.res[[4]])), kenton.res[[4]], col=1, pch=20, cex=1.25)


tmp <- qnorm(1 - 0.05/(2*10), sd=sqrt(summary(kenton.aov)[[1]][2,3] * (1-1/2)))
abline(h=tmp, lty=2)
abline(h=-tmp, lty=2)

tmp <- qnorm(1 - 0.05/(2*10), sd=sqrt(summary(kenton.aov)[[1]][2,3] * (1-1/3)))
abline(h=tmp, lty=2, col=4)
abline(h=-tmp, lty=2, col=4)