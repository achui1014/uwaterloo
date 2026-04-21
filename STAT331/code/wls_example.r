# STAT331 Ch3: Weighted Least Squares Example
library(HistData)
library(dplyr)

data(GaltonFamilies)

# Objective: Construct family-level data by calculating the average child height
# for each family
# * Fit linear regression model: average child height ~ average parent height
# * Use WLS to account for unequal variances across families in the regression

# select relevant columns
galton_simple <- GaltonFamilies %>%
    select(family, midparentHeight, childHeight, childNum)

# aggregate to family level
family_data <- galton_simple %>%
    # group by family
    group_by(family, midparentHeight) %>%
    summarise(
        # average height per family
        avg_childHeight = mean(childHeight),
        # number of children per family
        k = n()
    ) %>%
    ungroup() %>%
    as.data.frame()
head(family_data)

# fit Ordinary Least Squares
ols_fit <- lm(avg_childHeight ~ midparentHeight, data = family_data)
summary(ols_fit)

# fit Weighted Least Squares
# weights = k (# of children per family)
wls_fit <- lm(avg_childHeight ~ midparentHeight,
              data = family_data,
              weights = k)
summary(wls_fit)

# key takeaway: WLS accounts for unequal variances in family-level averages
# * larger families get more weight, improving estimate precision
# * CI intervals for the mean response are more accurate than OLS
# * the WLS line fits the family-level relationship more efficiently,
#   especially when family sizes vary