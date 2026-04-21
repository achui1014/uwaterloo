vax.dat <- read.csv("vaxCovid19.csv", header = TRUE)

vax.dat$date <- as.factor(vax.dat$date)
vax.dat$ageGrp <- as.factor(vax.dat$ageGrp)
vax.dat$PHU <- as.factor(vax.dat$PHU)

# part a) two-way ANOVA
vax.aov <- aov(doses ~ ageGrp + date + ageGrp*date, data = vax.dat)
summary(vax.aov)
# ageGrp and date are highly significant with p-values << 0.05

# part b) which ageGrp has the highest number of doses
model.tables(vax.aov, type = "means")
# age group 18-29 yrs has a mean of 416.7 doses per day per PHU

# perform multiple comparisons
TukeyHSD(vax.aov, "ageGrp")
# Since all pairwise comparisons involving the 18-29 yr old age group are
# statistically significant which makes sense since most older age groups
# have already gotten their vaccinations by this date.

# part c) which ageGrp has the lowest number of doses
# age group 80+ has a mean of 28.8 doses per day per PHU. When we perform
# multiple comparisons, the differences between the age groups are all
# statistically significant except for the one between the 70-79 group.
