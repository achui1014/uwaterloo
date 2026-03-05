ins <- read.csv("insurance.csv", header = T)

ins$sex <- factor(ins$sex) # female as reference cat
ins$smoker <- factor(ins$smoker) # yes as reference cat
ins$region <- factor(ins$region) # NE as reference cat

fit <- lm(charges ~ age + bmi + children + sex + smoker + region, data=ins)
summary(fit)

# reduce model to exclude region and assess its significance
reduced_fit <- lm(charges ~ age + bmi + children +sex + smoker, data = ins)
anova(fit, reduced_fit)