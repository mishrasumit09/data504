# code for topic 6
pacman::p_load(tidyverse, data.table)

df <- data.table(var1 = c("Prop wo toilet", "Prop w toilet"),
                 var2 = c(320,350))

summary.df<- df[, .(p.wo.toilet = var2/sum(var2))]

round(summary.df[1] + 
        1.96*sqrt((summary.df[1]*summary.df[2])/sum(df$var2)), 2)

round(summary.df[1] - 
        1.96*sqrt((summary.df[1]*summary.df[2])/sum(df$var2)), 2)


# How many people should you sample in order to cut the margin of
# error of a 95% confidence interval down to 1%.
(1.96^2*summary.df[1]*summary.df[2])/0.01^2


# Do these data provide convincing evidence that 
# more than 40\% of Indians have access to toilet?

stder1 <- sqrt((0.4*0.6)/670)
stder1

z.val1 <- (0.48 - 0.4)/stder1

pnorm(z.val1)


# Question 6.27

sleep <- c(rep("less than 6",70), rep("6 to 8", 310), rep("more than 8", 115))
driver_typ <- c(rep("Non-transportation",35), rep("Truck Drivers", 35), 
                rep("Non-transportation", 193), rep("Truck Drivers", 117),
                rep("Non-transportation", 64), rep("Truck Drivers",51))
q6.27 <- data.table(sleep, driver_typ)
q6.27$sleepfct <- factor(q6.27$sleep, 
                         levels = c("less than 6", "6 to 8", "more than 8"))


# Chi-Square test
# Example 1
# Dataset : mpg
# Variables: trans and class

table(mpg$trans, mpg$class)
mpgDT <- setDT(copy(mpg))

mpgDT <- mpgDT[, trans2:= ifelse(trans %like% "auto", "auto", "manual")]

t1 <- table(mpgDT$trans2, mpgDT$class)

t1.test <- chisq.test(t1)

t1.test$expected # gets you the expected frequency
t1.test$p.value # gets you the p-value

# you will see a warning (that's because the total number of 2-seater cars
# is just 5)

# Interpretation:
# we reject the null hypothesis for the Chi-square test of independence 
# this means that there is a significant relationship between 
# the type of transmission and class of car.

summary(t1) # this will also produce the same result


# Example 2
# iPod example from the textbook
# Recall that the following conditions must meet:
# Independence: Each case must be independent of all the other cases in the table.
# Sample size: Each cell count must have at least 5 expected cases.
# Degrees of freedom: We need 3 or more columns in the table.

problem <- c(rep("Disclose Problem", 61),
             rep("Hide Problem", 158))
questions <- c(rep("General",2), rep("Positive Assumption",23), 
               rep("Negative Assumption",36), rep("General",71), 
               rep("Positive Assumption",50), rep("Negative Assumption",37))
iPodExample <- data.table(problem,questions)

# plot the data
ggplot(data = iPodExample, aes(x = questions, fill = problem)) +
  geom_bar(position = "fill", color = "black") +
  xlab("\n Question") +
  ylab("Conditional Probability\n") +
  scale_fill_manual(values = c("#D81B60", "#004D40"),
                    label = c("Disclose", "Hide")) +
  theme_bw()

# use the canned function to perform the chi-square test
chisq.test(x = table(iPodExample$questions, iPodExample$problem), 
           correct = FALSE)

# now do it the expected count way
# calculate the expected count
# figure out the degrees of freedom
# use the general formula to compute the chi-square statistic

