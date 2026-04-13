# Ch4 Lecutre Examples

# 4.3.4 Estimating Ratios
# Estimating population ratio (R_U=\bary_U/\barx_U) under an SRS design
library("survey")

mu284.dat <- read.table("mu284.dat", header=T)
set.seed(20210518)
mu284.srs <- sample(mu284.dat$LABEL, 100, replace=F)
sort(mu284.srs)
mu284.srs <- mu284.dat[mu284.dat$LABEL %in% mu284.srs, ]


# Specifying the sampling design (SRS in the present case)
mu284.svy <- svydesign(ids=~1, probs=c(100/284),
    fpc = rep(284, nrow(mu284.srs)), data= mu284.srs)

svymean(~P85 + P75, mu284.svy)
tmp <- svyratio(~P85, ~P75, mu284.svy)

# Computing the 95% CI with t distribution
confint(tmp, df = nrow(mu284.srs)-1)
confint(tmp, df = degf(mu284.svy))

# Computing the 99% CI with t distribution
confint(tmp, df = degf(mu284.svy), level=0.99)

# Computing the 95% CI with Normal distribution
confint(tmp)

# 4.4.4 Using y_ra to estimate P85 in MU284
tmp <- svyratio(~P85, ~P75+CS82, mu284.svy)

# Using P75 as the auxiliary variable yields
mean(mu284.dat$P75)
# point estimate
mean(mu284.dat$P75) * coef(tmp)[1]
# variance
mean(mu284.dat$P75)^2 * tmp$var[1]
# computing the 95% CI with t distribution
mean(mu284.dat$P75) * confint(tmp, df = degf(mu284.svy)) [1, ]
# computing the 99% CI with t distribution
mean(mu284.dat$P75) * confint(tmp, df = degf(mu284.svy), level=0.99)[1, ]
# computing the 95% CI with Normal distribution
# (there are 99 df, so Norma and t practically give the same answer)
mean(mu284.dat$P75) * confint(tmp)[1, ]

# Comparing with the HT Estimator
tmp2 <- svymean(~P85, mu284.svy)
confint(tmp2, df = degf(mu284.svy))

# Comparing with the true value
mean(mu284.dat$P85)

# Using CS82 as the auxiliary variable
mean(mu284.dat$CS82)
#point estimate
mean(mu284.dat$CS82) * coef(tmp)[2]
# variance
mean(mu284.dat$CS82)^2 * tmp$var[2]
# computing the 95% CI with t distribution
mean(mu284.dat$CS82) * confint(tmp, df = degf(mu284.svy))[2, ]

# Notes:
# * the population mean of P85 is 29.36, and using P75 yields a point estimate
#   closer to that true population mean than using CS82.
# * the CI of y_ra using P75 is much narrower than the one obtained using CS82
# * this is not surprising since the correlation between P85 and P75 is much
#   greater than the one between P85 and CS82