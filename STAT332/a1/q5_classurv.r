library("survey")

data <- read.table("a1/classurv.txt", header = TRUE)

# remove missing GPA values coded as -9
data$GPA[data$GPA == -9] <- NA

# Define the survey design
dsgn <- svydesign(ids= ~1, probs =c(57/350), fpc = rep(350, nrow(data)), data = data)
df0 <- degf(dsgn)

#a) Estimate the average GPA of all students
a <- svymean(~GPA, dsgn, na.rm=TRUE)
confint(a, df = df0)

#b) Estimate average GPA of male students
#c) Estimate average GPA of female students
b_c <- svyby(~GPA, ~gender, dsgn, svymean, na.rm = TRUE)
confint(b_c, df = df0)

#d) Estimate the proportions of male/female students
d <- svymean(~factor(gender), dsgn)
confint(d, df = df0)

#e) Estimate the proportions of students that are freshman/sophomore/junior/senior/other
e_1 <- svyciprop(~I(year == 1), dsgn, method = "logit") # freshmen
e_2 <- svyciprop(~I(year == 2), dsgn, method = "logit") # sophomore
e_3 <- svyciprop(~I(year == 3), dsgn, method = "logit") # junior
e_4 <- svyciprop(~I(year == 4), dsgn, method = "logit") # senior
e_5 <- svyciprop(~I(year == 5), dsgn, method = "logit") # other

#f) Amongst freshmen, what are the proportions of male/female students
freshmen <- subset(dsgn, year == 1)
f_male <- svyciprop(~I(gender == 1), freshmen, method = "logit")
f_female <- svyciprop(~I(gender == 2), freshmen, method = "logit")

#g) Repeat f) but for seniors
seniors <- subset(dsgn, year == 4)
g <- svymean(~factor(gender), seniors)
confint(g, df =degf(seniors))

#h) Amongst the top performers (i.e. GPA >= 3.2), what are the proportions of male/female students
top_perf <- subset(dsgn, GPA >= 3.2)
h <- svymean(~factor(gender), top_perf, na.rm = TRUE)
confint(h, df = degf(top_perf))