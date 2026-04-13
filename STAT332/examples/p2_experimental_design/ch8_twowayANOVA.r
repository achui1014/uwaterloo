# Ch8: Two-Way ANOVA
bakery.dat <- data.frame(
    width = rep(c("Regular", "Wide"), each = 6),
    height = rep(rep(c("Bottom", "Middle", "Top"), each = 2), times = 2),
    cases = c(
        47, 43, 62, 68, 41, 39, # Regular
        46, 40, 67, 71, 42, 46  # Wide
    )
)

boxplot(cases ~ height, data = bakery.dat)
boxplot(cases ~ width, data = bakery.dat)
boxplot(cases ~ height*width, data = bakery.dat, cex.axis = 0.5)

# ANOVA table
bakery.aov <- aov(cases ~ width + height + width*height, data = bakery.dat)
summary(bakery.aov)

# Pairwise Comparisons
TukeyHSD(bakery.aov, conf.level = 0.95)
TukeyHSD(bakery.aov, "height", conf.level = 0.95)
plot(TukeyHSD(bakery.aov, "height"))

hayfever.dat <- data.frame(scan(what=list(hours=0, ingredient1=0, ingredient2=0, junk=0)))
    2.4      1      1      1
    2.7      1      1      2
    2.3      1      1      3
    2.5      1      1      4
    4.6      1      2      1
    4.2      1      2      2
    4.9      1      2      3
    4.7      1      2      4
    4.8      1      3      1
    4.5      1      3      2
    4.4      1      3      3
    4.6      1      3      4
    5.8      2      1      1
    5.2      2      1      2
    5.5      2      1      3
    5.3      2      1      4
    8.9      2      2      1
    9.1      2      2      2
    8.7      2      2      3
    9.0      2      2      4
    9.1      2      3      1
    9.3      2      3      2
    8.7      2      3      3
    9.4      2      3      4
    6.1      3      1      1
    5.7      3      1      2
    5.9      3      1      3
    6.2      3      1      4
    9.9      3      2      1
   10.5      3      2      2
   10.6      3      2      3
   10.1      3      2      4
   13.5      3      3      1
   13.0      3      3      2
   13.3      3      3      3
   13.2      3      3      4

hayfever.dat <- hayfever.dat[ , -4]   
   
library(car) # needed to use recode function
hayfever.dat$ingredient1 <- recode(hayfever.dat$ingredient1, 'c(1)="low"; c(2)="medium"; c(3)="high"; else=NA')
hayfever.dat$ingredient2 <- recode(hayfever.dat$ingredient2, 'c(1)="low"; c(2)="medium"; c(3)="high"; else=NA')
   
hayfever.aov <- aov(hours ~ ingredient1*ingredient2, data = hayfever.dat)
summary(hayfever.aov)

tmp <- model.tables(hayfever.aov, type = "means")
tmp <- tmp[[1]][[4]]

# re-ordering to rows & colummns to make the plots look more natural
tmp <- tmp[, c("low", "medium", "high")]
tmp <- tmp[c("low", "medium", "high"), ]

plot(1:3, tmp[1, ], ylim = c(min(tmp), max(tmp)), type = "l", xaxt = 'n',
    ylab = "Hours of Relief", xlab = "Ingredient # 2")
axis(1, at = 1:3, labels = c("low", "medium", "high"))
lines(1:3, tmp[2, ], col = 4)
lines(1:3, tmp[3, ], col = 2)
legend(1, 13, legend = c("low", "medium", "high"), col = c(1, 4, 2), lty = 1)
text(1.25, 13.2, "Ingredient # 1")

plot(1:3, tmp[, 1], ylim = c(min(tmp), max(tmp)), type = "l", xaxt = 'n',
    ylab = "Hours of Relief", xlab = "Ingredient # 1")
axis(1, at = 1:3, labels = c("low", "medium", "high"))
lines(1:3, tmp[, 2], col = 4)
lines(1:3, tmp[, 3], col = 2)
legend(1, 13, legend = c("low", "medium", "high"), col = c(1, 4, 2), lty = 1)
text(1.25, 13.2, "Ingredient # 2")

TukeyHSD(hayfever.aov, conf.level = 0.95)
plot(TukeyHSD(hayfever.aov, which = "ingredient1:ingredient2"))