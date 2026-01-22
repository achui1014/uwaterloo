directory <- "data"
filename <- file.path(directory, "agpop_data.csv")
agpop <- read.csv(filename, header = TRUE, stringsAsFactors = TRUE)

# Order Statistics
# The following graphs demonstrate how the ordering of attributes can be 
# informative
par(mfrow = c(1, 2))
y <- agpop$acres87[agpop$region == "NE"]
y <- y[y != -99] # omit missing values

## unordered plot
plot(y,
    pch = 19, col = adjustcolor("grey", alpha = 0.5),
    xlab = "Unordered",
    ylab = "Farming acres in 1987",
    main = "Counties in the North East USA \n by Farming acres in 1987"
)

## ordered plot
yrank <- rank(y, ties.method = "first") # ensure ties appear in data set order
plot(yrank, y,
    pch = 19, col = adjustcolor("grey", alpha = 0.5),
    xlab = "Ordered", 
    ylab = "Farming acres in 1987",
    main = "Counties in the North East USA \n Ordered by Farming acres in 1987"
)


summary(agpop) #summary of (some) population attributes
summary(factor(agpop$state)) # summarize one state at a time
summary(factor(agpop$region)) # summarize one region at a time

# negative indexing - omit the 1st, 2nd, and 15th variates
summary(agpop[, -c(1, 2, 15)])

# Missing data
# which values are missing can be determined with a logical query
missing92 <- agpop[, "acres92"] == -99
## missing92 is a logical vector of the same length as agpop[, "acres92"]
## TRUE in every position where -99 appeared and FALSE otherwise.
## The total number of missing values can be had by summing (because logical
## TRUE is treated as 1, and FALSE as 0)
sum(missing92)

# Alternatively, the 'which' function could be used to identify the row numbers
row_nums_missing <- which(agpop[, "acres92"] == -99)

# These values can be changed to NA by using these locations (either
# row_nums_missing or missing92) to identify the rows and replace the values
agpop[missing92, "acres92"] <- NA
agpop[agpop[, "acres87"] == -99, "acres87"] <- NA
agpop[agpop[, "acres82"] == -99, "acres82"] <- NA

# or simply (alternative and preferred method of the above computation)
var_names <- c("acres82", "acres87", "acres92")
for (nm in var_names) {
    agpop[agpop[, nm] == -99, nm] <- NA
}
summary(agpop[, var_names])
