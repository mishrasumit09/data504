# Multiple and logistic regression
pacman::p_load(tidyverse, data.table, openintro,
               broom, kableExtra, 
               moderndive, skimr, haven, wooldridge, ISLR)



# Running multiple regression model

# Let's model wage as a function of
# education and experience

m1 <- lm(wage ~ educ + exper, data = wage1)
summary(m1)

# model with multiple factors

m2 <- lm(hwy ~ factor(cyl), data = mpg)
summary(m2)

# model with factor and numeric variable

m3 <- lm(wage ~ educ + female, data = wage1)
summary(m3)


# textbook example 9.2
# many predictors
loans <- data.table(copy(loans_full_schema))
m4 <- lm(interest_rate ~ verified_income + debt_to_income + 
           total_credit_utilized + term + issue_month,
         data = loans)


# mario kart case
tidy(lm(total_pr ~ factor(cond) + stock_photo + duration + wheels, 
        data = mariokart))

summary(lm(total_pr ~ factor(cond) + stock_photo + duration + wheels,
           data = mariokart))


# interpreting coefficients



# estimating R-squared
summary(aov(m1))
Rsqd.m1 <- (1180 + 433)/(1180 + 433 + 5548)


# looking at adjusted R-squared

# Question 9.3
# Data: smoke
# bwt: birth weight
# smoke: whether mother smokes
# parity: whether child is first born
# gestation: length of pregnancy
# age: mother's age in years
# height: mother's height in inches
# weight: mother's weight in pounds
q9.3Dta <- data.table(copy(babies))
m5 <- lm(bwt ~ smoke + parity + gestation + age + height + weight, data = q9.3Dta)
summary(aov(m5))

# compute the following:
# total sum of squares
# unexplained sum of squares
# n - 1 
# n - k - 1


# Logistic model primer
# Data: Bertrand-Mullainathan
# Outcome: callback
# Predictor: Race

bmDta <- fread("data/bm_clean.csv") %>%
  .[, `:=`(callback = ifelse(call=="Yes",1,0),
           black = ifelse(race=="Black",1,0))]

summary(glm(callback ~ black, data=bmDta, family=binomial))


# Data: Default
# Outcome: default
# Predictor: student
defaultDta <- data.table(copy(Default)) %>%
  .[, `:=`(default10 = ifelse(default == "Yes",1,0),
           student10 = ifelse(student == "Yes",1,0))]

summary(glm(default10 ~ student10, data=defaultDta, family=binomial))
