# Ch10: Two-Level Factorial Design

# Ex 10.2.6 2^2 Designs
# subjects: persons 25-35 years old
# factor A = gender of subject (female, male)
# factor B = body fat of subject (high, low)
# reponse Y = exercise tolerance (in minutes until fatigue occurs while the 
# subject is performing on a bicycle apparatus)

tolerance <- scan()
24.1 
29.2 
24.6 
20.0  
21.9 
17.6 
14.6 
15.3 
12.3  
16.1 
9.3  
10.8 
17.6
18.8
23.2  
14.8 
10.3 
11.3
14.9
20.4
12.8
10.1
14.4
6.1 

exercise.dat <- data.frame(tolerance = tolerance, smoking=rep(c("light", "heavy"), c(12, 12)),
    fat=rep(rep(c("low", "high"), c(6,6)), 2), gender=rep(rep(c("male", "female"), c(3,3)), 4))

exercise.dat$smoking <- as.factor(exercise.dat$smoking)
exercise.dat$fat <- as.factor(exercise.dat$fat)
exercise.dat$gender <- as.factor(exercise.dat$gender)

exercise.aov <- aov(tolerance ~ fat*gender, data = exercise.dat)
exercise.means <- model.tables(exercise.aov, type = "means")[[1]]$'fat:gender'

# Contrast Effects
# Fat
effect_fat <- matrix(c(1, -1, 1, -1), 2, 2)
effect_fat <- (1/2) * sum(effect_fat * exercise.means)
SS_fat <- 6 * (effect_fat)^2

# Gender
effect_gender <- matrix(c(-1, -1, 1, 1), 2, 2)
effect_gender <- (1/2) * sum(effect_gender * exercise.means)
SS_gender <- 6 * (effect_gender)^2

# Interaction
effect_inter <- matrix(c(1, -1, -1, 1), 2, 2)
effect_inter <- (1/2) * sum(effect_inter * exercise.means)
SS_inter <- 6 * (effect_inter)^2

# Compare results above to aov function
summary(exercise.aov)

# same --> hence 2^2 factorial design is the same as two-way anova

# 2^3 Designs
# factor C = smoking (light, heavy)
# other factors remain as in the above 2^2 model
exercise.aov2 <- aov(tolerance ~ fat*gender*smoking, data = exercise.dat)
summary(exercise.aov2)

exercise.means2 <- model.tables(exercise.aov2, type = "means")[[1]]$'fat:gender:smoking'
exercise.means2 <- data.frame(means = as.vector(exercise.means2),
    gender = rep(rep(c("female", "male"), c(2, 2)), 2),
    fat = rep(c("high", "low"), 4),
    smoking = rep(c("heavy", "light"), c(4, 4)))
exercise.means2

# Estimate Contrast Effects
# Fat
effect_fat <- ifelse(exercise.means2$fat == "low", -1, 1)
effect_fat <- (1/4) * sum(effect_fat * exercise.means2$means)
effect_fat

SS_fat <- nrow(exercise.dat)/4 * effect_fat^2
SS_fat

# Gender
effect_gender <- ifelse(exercise.means2$gender == "female", -1, 1)
effect_gender <- (1/4) * sum(effect_gender * exercise.means2$means)
effect_gender

SS_gender <- nrow(exercise.dat)/4 * effect_gender^2
SS_gender

# Smoking
effect_smoking <- ifelse(exercise.means2$smoking == "light", -1, 1)
effect_smoking <- (1/4) * sum(effect_smoking * exercise.means2$means)
effect_smoking

SS_smoking <- nrow(exercise.dat)/4 * effect_smoking^2
SS_smoking

# Fat*Gender
effect_fat <- ifelse(exercise.means2$fat == "low", -1, 1)
effect_gender <- ifelse(exercise.means2$gender == "female", -1, 1)
effect_fat_gender <- effect_fat * effect_gender
effect_fat_gender <- (1/4) * sum(effect_fat_gender * exercise.means2$means)
effect_fat_gender
SS_fat_gender <- nrow(exercise.dat)/4 * effect_fat_gender^2
SS_fat_gender
# result is same as before (without smoking in the model)

# without 3-way interaction
exercise.aov3 <- aov(tolerance ~ fat + gender + smoking +
    fat:gender + fat:smoking + gender:smoking, data = exercise.dat)
summary(exercise.aov3)

# 2^4 Designs
# Bacterial Growth Example
log.count <- scan()

5.55
4.47
5.19
5.32
10.54
11.56
5.08
5.45
5.12
5.63
6.18
5.24
10.73
10.33
6.53
4.93

temperature <- rep(c("low", "high"), 8)
preservative <- rep(rep(c("low", "high"), c(2, 2)), 4)
moisture <- rep(rep(c("low", "high"), c(4, 4)), 2)
acidity <- rep(c("low", "high"), c(8, 8))

bacterialgrowth.dat <- data.frame(log.count, temperature, preservative, moisture, acidity)
bacterialgrowth.aov <- aov(log.count ~
    temperature*preservative*moisture*acidity, data = bacterialgrowth.dat)
summary(bacterialgrowth.aov)