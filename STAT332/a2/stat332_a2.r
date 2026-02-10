library("survey")
syc <- read.table("syc.txt", header = TRUE, sep = ",")

# Handle missing codes
syc$age[syc$age == 99] <- NA
syc$race[syc$race == 9] <- NA
syc$numarr[syc$numarr == 99] <- NA
syc$prviol[syc$prviol == 9] <- NA
syc$everdrug[syc$everdrug == 9] <- NA

# Survey design object
syc_des <- svydesign(
    ids = ~1, 
    strata = ~stratum,
    weights = ~finalwt,
    data = syc
)
df0 <- degf(syc_des)

# a) Estimate the average age of juveniles/young adults in custody.
a <- svymean(~age, syc_des, na.rm = TRUE)
confint(a, df = df0)

# b) Estimate the mean number of prior arrests.
b <- svymean(~prviol, syc_des, na.rm = TRUE)
confint(b, df = df0)

# c) Estimate the proportion of juveniles/young adults in custody that have
# used illegal drugs.
c <- svymean(~as.factor(everdrug), syc_des, na.rm =TRUE)
confint(c, df = df0)

# d) Estimate the proportion of juveniles/young adults in custody that were
# previously arrested for a violent crime
d <- svymean(~prviol, syc_des, na.rm = TRUE)
confint(d, df = df0)

# e) Estimate the proportion of juveniles/young adults in custody that have
# used illegal drugs and were previously arrested for a violent crime.
# create joint indicator variable
syc$vio_and_drug <- ifelse(syc$everdrug == 1 & syc$prviol == 1, 1, 0)
syc_des <- svydesign( # redefine design object
    ids = ~1, 
    strata = ~stratum,
    weights = ~finalwt,
    data = syc
)
e <- svymean(~vio_and_drug, syc_des, na.rm = TRUE)
confint(e, df = df0)

# f) Estimate the proportion of males amongst juveniles/young adults
# in custody.
f <- svymean(~factor(sex), syc_des, na.rm = TRUE)
confint(f, df = df0)

# g) Estimate the proportion of African Americans amongst 
# juveniles/young adults in custody.
g <- svymean(~factor(race), syc_des, na.rm = TRUE)
confint(g, df = df0)

# h) Estimate the proportion of African American males amongst juveniles/
# young adults in custody.
syc$black_male <- ifelse(syc$race == 2 & syc$sex == 1, 1, 0)
syc_des <- svydesign( # redefine design object
    ids = ~1, 
    strata = ~stratum,
    weights = ~finalwt,
    data = syc
)
h <- svymean(~black_male, syc_des, na.rm = TRUE)
confint(h, df = df0)