water <- read.table("data/water.txt", header = FALSE)
colnames(water) <- c("burner", "salt", "time")

# burner: (1 = right-back), (2 = right-front), (3 = left-back), (3 = left-front)
# salt measured in teaspoons (0, 2, 4, and 6 teaspoons)
# time to boiling (in minutes)

# produce ANOVA table
water.aov <- aov(time ~ burner * salt, data = water)
aov_tab <- summary(water.aov)

