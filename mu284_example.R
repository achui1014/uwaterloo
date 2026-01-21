mu284.dat <- read.table("/Users/ashleychui/Desktop/Winter '26/stat332/mu284.dat", header = T)
head(mu284.dat) # view top rows to visually assess that the data was correctly read
dim(mu284.dat) # ensure the correct number of rows and columns

# Drawing an SRS sample of size 100:
# ====================================
set.seed(20210518)

mu284.srs <- sample(mu284.dat$LABEL, 100, replace = F) # without replacement (F)
sort(mu284.srs)

mu284.srs <- mu284.dat[mu284.dat$LABEL %in% mu284.srs, ]

# confirming that our SRS sample consists of 100 observations
dim(mu284.srs)

# Estimating population means (under SRS)
# =======================================

# P75 = 1975 population (in thousands)
# ------------------------------------
ht.mean <- mean(mu284.srs$P75)
ht.mean

nrow(mu284.srs)
var.ht.mean <- (1/nrow(mu284.srs) - 1/284) * var(mu284.srs$P75)
sqrt(var.ht.mean)

# confidence interval
ht.mean + c(-1, 1) * qt(0.975, nrow(mu284.srs) - 1) * sqrt(var.ht.mean)
mean(mu284.dat$P75) # to compare with the true value

# P85 = 1985 population (in thousands)
# -------------------------------------
ht.mean <- mean(mu284.srs$P85)

var.ht.mean <- (1/nrow(mu284.srs) - 1/284) * var(mu284.srs$P85)
sqrt(var.ht.mean)

ht.mean + c(-1, 1) * qt(0.975, nrow(mu284.srs)-1) * sqrt(var.ht.mean)
mean(mu284.dat$P85) # to compare with the true value

# RM85 = Revenues from the 1985 municipal taxation (in millions of kronor)
# -------------------------------------------------------------------------
ht.mean <- mean(mu284.srs$RM85)
var.ht.mean <- (1/nrow(mu284.srs) - 1/284) * var(mu284.srs$RM85)

ht.mean + c(-1, 1) * qt(0.975, nrow(mu284.srs)-1) * sqrt(var.ht.mean)
mean(mu284.dat$RM85) # to compare with the true value

# Estimating population totals
# ====================================

# P75 = 1975 population (in thousands)
# ------------------------------------

ht.total <- 284*mean(mu284.srs$P75)
var.ht.total <- 284^2 * (1/nrow(mu284.srs) - 1/284) * var(mu284.srs$P75)
sqrt(var.ht.total)

ht.total + c(-1 ,1) * qt(0.975, nrow(mu284.srs)-1) * sqrt(var.ht.total)
sum(mu284.dat$P75) # to compare with the true value

# P85 = 1985 population (in thousands)
# ------------------------------------
ht.total <- 284*mean(mu284.srs$P85)
var.ht.total <- 284^2 * (1/nrow(mu284.srs) - 1/284) * var(mu284.srs$P85)
sqrt(var.ht.total)
ht.total + c(-1 ,1) * qt(0.975, nrow(mu284.srs)-1) * sqrt(var.ht.total)
sum(mu284.dat$P85) # to compare with the true value

# RM85 = Revenues from the 1985 municipal taxation (in millions of kronor)
# ------------------------------------------------------------------------
ht.total <- 284*mean(mu284.srs$RM85)
var.ht.total <- 284^2 * (1/nrow(mu284.srs) - 1/284) * var(mu284.srs$RM85)

ht.total + c(-1 ,1) * qt(0.975, nrow(mu284.srs)-1) * sqrt(var.ht.total)
sum(mu284.dat$RM85) # to compare with the true value

# Estimating population proportions
# =========================================
# creating the prop25 variable
mu284.dat$prop25 <- ifelse(mu284.dat$P75<25, 1, 0)
mu284.srs$prop25 <- ifelse(mu284.srs$P75<25, 1, 0)

ht.prop <- mean(mu284.srs$prop25)
var.ht.prop <- (1/nrow(mu284.srs) - 1/nrow(mu284.dat)) * nrow(mu284.srs)/(nrow(mu284.srs)-1) *
  ht.prop*(1 - ht.prop)
sqrt(var.ht.prop)

var.ht.prop <- (1/nrow(mu284.srs) - 1/nrow(mu284.dat)) * var(mu284.srs$prop25)

ht.prop + c(-1 ,1) * qt(0.975, nrow(mu284.srs)-1) * sqrt(var.ht.prop)
mean(mu284.dat$prop25) # to compare with the true value