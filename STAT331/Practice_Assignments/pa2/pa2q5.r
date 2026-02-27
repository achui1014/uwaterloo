data <- data.frame(
    TV = c(-1, 0, 0, 1, -1, 0, 0, 1),
    Newspaper = c(0, -1, 1, 0, 0, -1, 1, 0),
    Retailer = factor(c("Absent", "Absent", "Absent", "Absent", "Present",
                "Present", "Present", "Present")),
    Change = c(1.2, 1.5, 2.2, 2.3, 1.9, 1.4, 2.8, 2.3)
)

fit <- lm(Change ~ TV + Newspaper + Retailer, data = data)
summary(fit)

# prediction interval for part d
new <- data.frame(TV = 0, Newspaper = 2, Retailer = "Absent")
predict(fit, new, interval = "prediction", level = 0.95)