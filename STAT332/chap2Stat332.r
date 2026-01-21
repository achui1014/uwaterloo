# Examples for Chapter 2 
# Stat 332: Sampling and Experimental Design (Winter 2026)
#
# C. Boudreau
# Last updated: Jan 19, 2026


# *** NOTE/WARNING:
# Because the SRS sample is randomly generated, it's possible that the numbers in this file do not correspond to the ones in the videos or the slides. Likewise, it's possible that you don't get the same numbers if you run this R code. Hence, the focus should be on the code, as opposed to the results.


setwd("~/Documents/Teaching/Stat332/Slides/RCode")
rm(list=ls()); load(".RData") 




# Creating the dataset:
# =====================

mu284.dat <- read.table("~/Documents/Teaching/Stat332/Datasets/mu284.dat", header=T)

> head(mu284.dat)
  LABEL P85 P75  RM85 CS82 SS82 S82 ME84 REV84 REG CL
1     1  33  27   288   13   24  49 2135  2836   1  1
2     2  19  15   139   14   12  41  957  2035   1  1
3     3  26  20   196   12   14  41 1530  6030   1  1
4     4  19  15   159   12   19  41 1059  4704   1  1
5     5  56  52   536   20   27  61 3951  5183   1  1
6     6  16  15   134   16   12  41  918  2157   1  2
> dim(mu284.dat)
[1] 284  11



# Drawing an SRS sample of size 100:
# ===================================

set.seed(20210518)

mu284.srs <- sample(mu284.dat$LABEL, 100, replace=F)
sort(mu284.srs)

mu284.srs <- mu284.dat[mu284.dat$LABEL %in% mu284.srs, ]

> head(mu284.srs)
   LABEL P85 P75 RM85 CS82 SS82 S82  ME84 REV84 REG CL
3      3  26  20  196   12   14  41  1530  6030   1  1
6      6  16  15  134   16   12  41   918  2157   1  2
8      8  66  54  517   15   32  61  4345  5246   1  2
12    12  20  14  155   10   21  41  1312  1899   1  3
16    16 653 671 6263   34   41 101 45324 59877   1  4
20    20  49  55  412   20   27  61  3240  3976   1  4

> dim(mu284.srs)
[1] 100  11


# Estimating population means (under SRS):
# =========================================

# P75 = 1975 population (in thousands)   
> ht.mean <- mean(mu284.srs$P75)
> ht.mean
[1] 27.58

> var.ht.mean <- (1/nrow(mu284.srs) - 1/284) * var(mu284.srs$P75)
> var.ht.mean
[1] 5.235869
> sqrt(var.ht.mean)
[1] 2.288202

> ht.mean + c(-1 ,1) * qt(0.975, nrow(mu284.srs)-1) * sqrt(var.ht.mean)
[1] 23.03971 32.12029

> mean(mu284.dat$P75) # to compare with the true value
[1] 28.80986



# P85 = 1985 population (in thousands)   
> ht.mean <- mean(mu284.srs$P85)
> ht.mean
[1] 34.24

> var.ht.mean <- (1/nrow(mu284.srs) - 1/284) * var(mu284.srs$P85)
> var.ht.mean
[1] 38.75127
> sqrt(var.ht.mean)
[1] 6.225052

> ht.mean + c(-1 ,1) * qt(0.975, nrow(mu284.srs)-1) * sqrt(var.ht.mean)
[1] 21.88815 46.59185

> mean(mu284.dat$P85) # to compare with the true value
[1] 29.36268


# RM85 = Revenues from the 1985 municipal taxation (in millions of kronor)     
> ht.mean <- mean(mu284.srs$RM85)
> ht.mean
[1] 215.94

> var.ht.mean <- (1/nrow(mu284.srs) - 1/284) * var(mu284.srs$RM85)
> var.ht.mean
[1] 381.708
> sqrt(var.ht.mean)
[1] 19.53735

> ht.mean + c(-1 ,1) * qt(0.975, nrow(mu284.srs)-1) * sqrt(var.ht.mean)
[1] 177.1737 254.7063


> mean(mu284.dat$RM85) # to compare with the true value
[1] 244.993




# Estimating population totals (under SRS):
# ========================================

# P75 = 1975 population (in thousands)   
> ht.total <- 284*mean(mu284.srs$P75)
> ht.total
[1] 9670.2

> var.ht.total <- 284^2 * (1/nrow(mu284.srs) - 1/284) * var(mu284.srs$P75)
> var.ht.total
[1] 3329218
> sqrt(var.ht.total)
[1] 1824.614

> ht.total + c(-1 ,1) * qt(0.975, nrow(mu284.srs)-1) * sqrt(var.ht.total)
[1]  6049.769 13290.631

> sum(mu284.dat$P75) # to compare with the true value
[1] 8182


# P85 = 1985 population (in thousands)   
> ht.total <- 284*mean(mu284.srs$P85)
> ht.total
[1] 9724.16

> var.ht.total <- 284^2 * (1/nrow(mu284.srs) - 1/284) * var(mu284.srs$P85)
> var.ht.total
[1] 3125522
> sqrt(var.ht.total)
[1] 1767.915


> ht.total + c(-1 ,1) * qt(0.975, nrow(mu284.srs)-1) * sqrt(var.ht.total)
[1]  6216.234 13232.086

> sum(mu284.dat$P85) # to compare with the true value
[1] 8339



# RM85 = Revenues from the 1985 municipal taxation (in millions of kronor)
> ht.total <- 284*mean(mu284.srs$RM85)
> ht.total
[1] 61326.96

> var.ht.total <- 284^2 * (1/nrow(mu284.srs) - 1/284) * var(mu284.srs$RM85)
> var.ht.total
[1] 30787037
> sqrt(var.ht.total)
[1] 5548.607

> ht.total + c(-1 ,1) * qt(0.975, nrow(mu284.srs)-1) * sqrt(var.ht.total)
[1] 50317.32 72336.60


> sum(mu284.dat$RM85) # to compare with the true value
[1] 69578



# Estimating population proportions:
# ==================================

# creating the prop25 variable
mu284.dat$prop25 <- ifelse(mu284.dat$P75<25, 1, 0)
mu284.srs$prop25 <- ifelse(mu284.srs$P75<25, 1, 0)

> ht.prop <- mean(mu284.srs$prop25)
> ht.prop
[1] 0.64

> var.ht.prop <- (1/nrow(mu284.srs) - 1/nrow(mu284.dat)) * nrow(mu284.srs)/(nrow(mu284.srs)-1) * ht.prop*(1 - ht.prop)
> var.ht.prop
[1] 0.00150781
> sqrt(var.ht.prop)
[1] 0.03883054
> var.ht.prop <- (1/nrow(mu284.srs) - 1/nrow(mu284.dat)) * var(mu284.srs$prop25)
> var.ht.prop
[1] 0.00150781

> ht.prop + c(-1 ,1) * qt(0.975, nrow(mu284.srs)-1) * sqrt(var.ht.prop)
[1] 0.5629518 0.7170482

> mean(mu284.dat$prop25) # to compare with the true value
[1] 0.6514085


# The survey package:
# ===================

# installing the survey package (you need to do the following line a SINGLE TIME)
install.packages("survey")


# loading the survey package (you'll need to do the following EVERY TIME YOU LAUNCH R)
library("survey")


# specifying the sampling design (SRS in the present case),
#    the pi_k's and the finite population correction (i.e., N)
mu284.svy <- svydesign(ids=~1, probs=c(100/284),
               fpc=rep(284, nrow(mu284.srs)), data=mu284.srs)


> summary(mu284.svy)
Independent Sampling design
svydesign(ids = ~1, probs = c(100/284), fpc = rep(284, nrow(mu284.srs)), 
    data = mu284.srs)
Probabilities:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.3521  0.3521  0.3521  0.3521  0.3521  0.3521 
Population size (PSUs): 284 
Data variables:
 [1] "LABEL"  "P85"    "P75"    "RM85"   "CS82"   "SS82"   "S82"    "ME84"   "REV84"  "REG"    "CL"     "prop25"

> 100/284
[1] 0.3521127


# Estimating population means:
# ----------------------------


> svymean(~P75, mu284.svy)
     mean     SE
P75 34.05 6.4247

# check
> mean(mu284.srs$P75)
[1] 34.05

> tmp <- svymean(~P75, mu284.svy)
> tmp
     mean     SE
P75 27.53 2.6017

# computing the CI (default is 95%)
> confint(tmp)
       2.5 %   97.5 %
P75 21.45782 46.64218

# computing the 99% CI
> confint(tmp, level=0.99)
       0.5 %   99.5 %
P75 17.50107 50.59893

# checking if the confint function uses quantile from the Normal or t distribution 
> c(coef(tmp)) + c(-1, 1) * qnorm(0.975) * c(SE(tmp))
[1] 21.45782 46.64218
> c(coef(tmp)) + c(-1, 1) * qt(0.975, nrow(mu284.srs)-1) * c(SE(tmp))
[1] 21.302 46.798

# From the above, confint (when used in conjuction with svymean) uses quantile from 
#    Normal distribution dy default


# now using the quantile from the t distribution (default is 95%)
> confint(svymean(~P75, design=mu284.svy), "P75", df=99)
     2.5 % 97.5 %
P75 21.302 46.798

> confint(svymean(~P75, design=mu284.svy), "P75", df=nrow(mu284.srs)-1)
     2.5 % 97.5 %
P75 21.302 46.798
> degf(mu284.svy)
[1] 99
> confint(svymean(~P75, design=mu284.svy), "P75", df=degf(mu284.svy))
     2.5 % 97.5 %
P75 21.302 46.798

# computing 99% CI using quantile from the t distribution
> confint(svymean(~P75, design=mu284.svy), "P75", df=degf(mu284.svy), level=0.99)
       0.5 %   99.5 %
P75 17.17614 50.92386



# Doing more than one variable at the same time
> tmp <- svymean(~P75+P85, mu284.svy)
> tmp
     mean     SE
P75 34.05 6.4247
P85 34.24 6.2251

# 95% CIs (using quantile from Normal distribution)
> confint(tmp)
       2.5 %   97.5 %
P75 21.45782 46.64218
P85 22.03912 46.44088

# 95% CIs (using quantile from t-distribution)
> confint(tmp, c("P75", "P85"), df=degf(mu284.svy))
       2.5 %   97.5 %
P75 21.30200 46.79800
P85 21.88815 46.59185

> confint(tmp, names(tmp), df=degf(mu284.svy))
       2.5 %   97.5 %
P75 21.30200 46.79800
P85 21.88815 46.59185



# Estimating population totals:
# -------------------------------

tmp <- svytotal(~P75, mu284.svy)

> tmp
     total     SE
P75 9670.2 1824.6
> confint(tmp, names(tmp), df=nrow(mu284.srs)-1)
       2.5 %   97.5 %
P75 6049.769 13290.63


# Doing more than one variable at the same time
tmp <- svytotal(~P75+P85, mu284.svy)

> tmp
     total     SE
P75 9670.2 1824.6
P85 9724.2 1767.9
> confint(tmp, names(tmp), df=nrow(mu284.srs)-1)
       2.5 %   97.5 %
P75 6049.769 13290.63
P85 6216.234 13232.09
> confint(tmp, names(tmp), df=degf(mu284.svy))
       2.5 %   97.5 %
P75 6049.769 13290.63
P85 6216.234 13232.09
> confint(tmp, names(tmp), df=degf(mu284.svy), level=0.99)
       0.5 %   99.5 %
P75 4878.023 14462.38
P85 5080.899 14367.42


# Estimating population proportions:
# -----------------------------------

# Note that proportions are a special case of the mean, and we can thus also use svymean
tmp <- svymean(~prop25, mu284.svy)
> tmp
       mean     SE
prop25 0.64 0.0388
> confint(tmp, names(tmp), df=degf(mu284.svy))
           2.5 %    97.5 %
prop25 0.5629518 0.7170482


> svyciprop(~I(prop25==1), mu284.svy, method="mean")
                      2.5% 97.5%
I(prop25 == 1) 0.640 0.563  0.72


> svyciprop(~I(prop25==1), mu284.svy, method="logit")
                    2.5% 97.5%
I(prop25 == 1) 0.64 0.56  0.71

# 99% CI
> confint(tmp, names(tmp), df=degf(mu284.svy), level=0.99)
           0.5 %    99.5 %
prop25 0.5380153 0.7419847
> svyciprop(~I(prop25==1), mu284.svy, method="mean", level=0.99)
                      0.5% 99.5%
I(prop25 == 1) 0.640 0.538  0.74


> svyciprop(~I(prop25==1), mu284.svy, method="mean")
                      2.5% 97.5%
I(prop25 == 1) 0.640 0.563  0.72
> svyciprop(~I(P75<25), mu284.svy, method="mean")
                   2.5% 97.5%
I(P75 < 25) 0.640 0.563  0.72


