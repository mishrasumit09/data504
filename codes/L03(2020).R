# Code for lecture 03
# Textbook Chapter 1

pacman::p_load(tidyverse, data.table, janitor)

# read the case dataset
stents <- fread("data/stents.csv")
head(stents)
tail(stents)

# create table 1.2 from the book
table(stents$group, stents$outcome30days)
table(stents$group, stents$outcome365days)

# data.table solution (slightly complex)
tab1.2 <- 
  groupingsets(
    stents,
    j = .N,
    by = c("group", "outcome30days", "outcome365days"),
    sets = list(
      c("group", "outcome30days"),
      c("group", "outcome365days")
    )
  )



# (2-way table) using tabyl
t1 <- stents %>%
  tabyl(group, outcome30days) %>%
  adorn_totals("row", 
               fill = "-", 
               na.rm = TRUE, name = "Total Cases") %>%
  rename(Group = group)

t2 <- stents %>%
  tabyl(group, outcome365days) %>%
  adorn_totals("row", 
               fill = "-", 
               na.rm = TRUE, name = "Total Cases") %>%
  rename(Group = group)

table1.2 <-
  merge(t1, t2, by = "Group") %>%
  arrange(factor(Group, 
                 levels = c("treatment", "control","Total Cases"))) %>%
  setDT()

# Calculate the proportion of patients in the treatment group
# who had a stroke at the end of 365 days
table1.2[Group=="treatment", 
         prop := round(stroke.y/(stroke.y + `no event.y`)*100)][]

# Calculate the proportion of patients in the control group
# who had a stroke at the end of 365 days
table1.2[Group=="control",
         prop := round(stroke.y/(stroke.y + `no event.y`)*100)][]


# Compute the difference between these two numbers
table1.2$prop[3] = table1.2$prop[1] - table1.2$prop[2]
table1.2[]

# Example 2: Toss of a coin
set.seed(1L)
coin <- c("head", "tail") # generate a coin in R
sample(coin, size=1, rep=T) # flip the coin once
sample(coin, size=100, rep=T) # flip the coin 100 times
tosses <- sample(coin, size=100, rep=T)
table(tosses)  # frequency table


# Exercise: Gradebook

roll_num <- c(1:5)
stu_name <- sample(LETTERS[1:26], size=5, rep=T)
assignment1_max <- rep(20,5)
assignment2_max <- rep(20,5)
exam_max <- rep(60,5)
assignment1_score <- sample(10:20, size=5, rep=T)
assignment2_score <- sample(10:20, size=5, rep=T)
exam_score <- sample(40:60, size=5, rep=T)

courseDB <- data.frame(roll_num,stu_name,assignment1_max,
                  assignment2_max, exam_max, assignment1_score,
                  assignment2_score, exam_score)


# Load county dataset
url <- "https://www.openintro.org/data/csv/county.csv"
county <- fread(url)
names(county)
head(county)

# example of numerical variables
# continuous variables
county %>% summary(per_capita_income)

facDB <- data.frame(rating =  c(1,2,3,4,5),
                    propn = c(5,30,40,20,5))
# the column rating is a discrete variable

# example of categorical variables
table(county$state)

# example of ordinal variable
x <- c("low", "medium", "high")
table(county$median_edu)


# relationship between variables
# we are going to use plot command
xlim <- c(0, max(county$median_hh_income, na.rm = T))
plot(county$median_hh_income,county$pop_change, 
     col = COL[1,3],
     xlab = "Median Household Income", 
     ylab = "Population Change",
     xlim = xlim,
     ylim = c(-15,25),
     cex  = 0.7)
