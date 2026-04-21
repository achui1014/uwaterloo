library(ISLR)
library(leaps)
library(glmnet)

data(Hitters)
Hitters <- na.omit(Hitters)

# Forward Selection
null_model <- lm(Salary ~ 1, data = Hitters)
full_model <- lm(Salary ~ ., data = Hitters)

a1 <- add1(null_model, scope = full_model, test = "F")
a1_sorted <- a1[order(a1$'Pr(>F)'), ]
head(a1_sorted)

model <- update(null_model, . ~ . + CRBI)
a2 <- add1(model, scope = full_model, test = "F")
a2_sorted <- a2[order(a2$'Pr(>F)'), ]
head(a2_sorted)

model <- update(model, . ~ . + Hits)
a3 <- add1(model, scope = full_model, test = "F")
a3_sorted <- a3[order(a3$'Pr(>F)'), ]
head(a3_sorted)

model <- update(model, . ~ . + PutOuts)
a4 <- add1(model, scope = full_model, test = "F")
a4_sorted <- a4[order(a4$'Pr(>F)'), ]
head(a4_sorted)

model <- update(model, . ~ . + Division)
a5 <- add1(model, scope = full_model, test = "F")
a5_sorted <- a5[order(a5$'Pr(>F)'), ]
head(a5_sorted)

model <- update(model, . ~ . + AtBat)
a6 <- add1(model, scope = full_model, test = "F")
a6_sorted <- a6[order(a6$'Pr(>F)'), ]
head(a6_sorted)

model <- update(model, . ~ . + Walks)
a7 <- add1(model, scope = full_model, test = "F")
a7_sorted <- a7[order(a7$'Pr(>F)'), ]
head(a7_sorted)

summary(model)$coef

# Backward Elimination
r1 <- drop1(full_model, test = "F")
r1_sorted <- r1[order(r1$'Pr(>F)', decreasing = TRUE), ]
head(r1_sorted)

model <- update(full_model, . ~ . - CHmRun)
r2 <- drop1(model, test = "F")
r2_sorted <- r2[order(r2$'Pr(>F)', decreasing = TRUE), ]
head(r2_sorted)

model <- update(model, . ~ . - Years)
r3 <- drop1(model, test = "F")
r3_sorted <- r3[order(r3$'Pr(>F)', decreasing = TRUE), ]
head(r3_sorted)

model <- update(model, . ~ . - NewLeague)
r4 <- drop1(model, test = "F")
r4_sorted <- r4[order(r4$'Pr(>F)', decreasing = TRUE), ]
head(r4_sorted)

model <- update(model, . ~ . - RBI)
r5 <- drop1(model, test = "F")
r5_sorted <- r5[order(r5$'Pr(>F)', decreasing = TRUE), ]
head(r5_sorted)

model <- update(model, . ~ . - CHits)
r6 <- drop1(model, test = "F")
r6_sorted <- r6[order(r6$'Pr(>F)', decreasing = TRUE), ]
head(r6_sorted)

model <- update(model, . ~ . - HmRun)
r7 <- drop1(model, test = "F")
r7_sorted <- r7[order(r7$'Pr(>F)', decreasing = TRUE), ]
head(r7_sorted)

model <- update(model, . ~ . - Errors)
r8 <- drop1(model, test = "F")
r8_sorted <- r8[order(r8$'Pr(>F)', decreasing = TRUE), ]
head(r8_sorted)

model <- update(model, . ~ . - Runs)
r9 <- drop1(model, test = "F")
r9_sorted <- r9[order(r9$'Pr(>F)', decreasing = TRUE), ]
head(r9_sorted)

model <- update(model, . ~ . - League)
r10 <- drop1(model, test = "F")
r10_sorted <- r10[order(r10$'Pr(>F)', decreasing = TRUE), ]
head(r10_sorted)

model <- update(model, . ~ . - Assists)
r11 <- drop1(model, test = "F")
r11_sorted <- r11[order(r11$'Pr(>F)', decreasing = TRUE), ]
head(r11_sorted)

model <- update(model, . ~ . - CAtBat)
r12 <- drop1(model, test = "F")
r12_sorted <- r12[order(r12$'Pr(>F)', decreasing = TRUE), ]
head(r12_sorted)

summary(model)$coef

# all subset regression
# fit all possible models with up to 19 predictors
regit.full <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
reg.summary <- summary(regit.full)

# Plot Adjusted R^2
plot(reg.summary$adjr2, type = "b",
    xlab = "Number of predictors", ylab = "Adjusted R^2",
    main = "Adjusted R^2 vs # of Variables")
abline(v = which.max(reg.summary$adjr2), col = "red", lty = 2)
text(which.max(reg.summary$adjr2), max(reg.summary$adjr2),
    labels = paste0("Max at ", which.max(reg.summary$adjr2)), pos = 1)

# Best subset by Adjusted R^2
# (Find the index of the model with the maximum adjusted R^2)
best_adjr2 <- which.max(reg.summary$adjr2)
# (names of predictors in the best model)
names(coef(regit.full, best_adjr2))

# Plot BIC
plot(reg.summary$bic, type = "b",
    xlab = "Number of Predictors", ylab = "BIC",
    main = "BIC vs. # of Variables")
abline(v = which.min(reg.summary$bic), col = "red", lty = 2)
text(which.min(reg.summary$bic), min(reg.summary$bic),
    labels = paste0("Min at ", which.min(reg.summary$bic)), pos = 2)

# Bets subset by BIC
best_bic <- which.min(reg.summary$bic)
names(coef(regit.full, best_bic))