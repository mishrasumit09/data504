# Packages for today's class:
# tidyverse: we are going to use pivot_* functions from this package
# data.table
# janitor: we will use functions to clean column names, etc.
pacman::p_load(tidyverse, data.table, readxl, janitor)

# We will learn the following today:
# 1. How to reshape datasets
# 2. How to create pivot-tables in R.
# 3. How to plot variables using ggplot

#-----------------------------------------------------------------#
# Before we start working on each of these,
# let me introduce the pipe operator `%>%`
# which will make your code more readable.

# Consider the following task:
# create a set of numbers
# take the average
# round it off

round(mean(1:100))

# using pipe:
# you start off with step 1
# add a pipe
# apply step 2 (compute the mean)
# add another pipe
# apply step 3 (round off the mean)
1:100 %>% 
  mean() %>%  # compute the mean
  round()     # round off the number

#-----------------------------------------------------------------#
# 1. Reshaping datasets

# There are two types of operations:
#              a) wide to long (pivot_longer)
#              b) long to wide (pivot_wider)

classDB <- data.table(
  name = c("Jaya", "Sushma", "Arun", "Uruj"),
  test1 = c(12,20,14,16),
  test2 = c(20,15,19,18),
  midterm = c(40,47,48,50),
  endterm = c(30,24,29,28))

# This dataset is currently in a wide format
# One row per each student.
# We will transform this data such that:
# there is a column for assessment type, and
# another column with scores associated with 
# each asssessment type.
cDBlong <-
  classDB %>% pivot_longer(cols= test1:endterm,
                         names_to = "assessment_type",
                         values_to = "score")

# you want to return to your original dataset?
# we will use pivot_wider to do that
cDBwide <- 
  cDBlong %>% pivot_wider(names_from = assessment_type,
                        values_from = score)


#-----------------------------------------------------------------#
# 2. Creating pivot tables in R

# We are going to use the starwars dataset

swDT <- data.table(copy(starwars))

#' 
#' Compute the average height and mass
#' by species, sex, and eyecolor.

swDT[, .(aveHT = mean(height, na.rm = T), 
         aveMS = mean(mass, na.rm = T)), 
     by = c("species", "sex", "eye_color")]

# (suppose that you are trying to calculate average
#  height, and mass by species and sex.)
# `cube()`: will give you the summary by group in the following manner:
#  i) summary by species and sex.
# ii) summary by species. (summary 1)
# iii) summary by sex.    (summary 2)
# iv) overall summary.    (grand summary)

st1 <- 
  cube(swDT,
       .(aveHT = mean(height, na.rm = T),
         aveMS = mean(mass, na.rm = T)),
       by = c("species", "sex")
  ) %>%
  .[order(species)] # sort by species
st1


# Using more dimensions
# With 3 dimensions:
st2 <- cube(
  swDT, 
  .(aveHT = mean(height, na.rm = T),
    aveMS = mean(mass, na.rm = T)),
  by = c("species", "sex", "eye_color")
) %>%
  .[order(species)]
st2



# customizing grouping sets:
# what if you want average height for 
# starwars characters by each of the variables:
# i.species, ii.sex, and iii.eye_color (separately)
groupingsets(
  swDT,
  j = .(aveHT = mean(height, na.rm=T)),
  by = c("species", "sex", "eye_color"),
  sets = list(c("species"), 
              c("sex"),
              c("eye_color"))
) %>%
  .[order(species)]


# what if we want two-dimensional summary
groupingsets(
  swDT,
  j = .(aveHT = mean(height, na.rm=T)),
  by = c("species", "sex", "eye_color"),
  sets = list(c("species", "sex"), 
              c("species", "eye_color"),
              c("sex","eye_color"))
) %>%
  .[order(species)]


# `rollup()`
# a great tool to get grouped summary in
# a particular order.
# in the following example, we compute
# average heights in the following order:
# - species + sex + eye_color
# - species + sex
# - species
rollup(swDT, mean(height, na.rm = T),
       by = c("species", "sex", "eye_color")) %>%
  .[order(species)]

#-----------------------------------------------------------------#
# 3. creating graphs using ggplot

# Dataset: mpg

# Before we get started with ggplot, it is useful
# to spend some time on the base R function plot()
# the function plot() has the following arguments:
#  - x: the variable on the x-axis
#  - y: the variable on the y-axis
#  - type: the type of the plot (p, l, h, etc.)
#  - main: a title for the graph
#  - sub: a subtitle 
#  - xlab: a title for the x-axis
#  - ylab: a title for the y-axis
#  - col: color for the graph
plot(mpg$displ, mpg$hwy)
plot(mpg$displ, mpg$hwy, 
     col = "purple",                             # add color
     main = "My first graph in R",               # title
     xlab = "Engine displacement",               # x-axis label
     ylab = "Miles per gallon")                  # y-axis label


# let's do this with ggplot
# the syntax to ggplot is quite simple
# (we will use pipe in this exercise)
# let's say you have a data frame called df
# you can create a graph by typing:
# df %>% ggplot() + geom_THE TYPE OF PLOT YOU WANT()
#                 + SOME BELLS AND WHISTLES
# 

# Our first ggplot: scatterplot
# geom_point()

mpg %>% ggplot() +
  geom_point(aes(x = displ, y = hwy),
             color = "purple")


# let's add labels to the graph
# we will add labs() to ggplot()
mpg %>% ggplot() +
  geom_point(aes(x = displ, y = hwy),
             color = "purple") +
  labs(x = "Displacement",
       y = "Miles per gallon")


# Line graph
economics %>%
  ggplot() +
  geom_line(aes(x = date, y = unemploy)) +
  theme_bw()


# Histogram: geom_histogram()
mpg %>% ggplot() + 
  geom_histogram(aes(x = hwy),
                 binwidth = 5)


# Let's make this histogram prettier:
# we will use fill and color options
# we can also change the theme!
mpg %>% ggplot() + 
  geom_histogram(aes(x = hwy),
                 fill = "blue",
                 color = "seagreen3",
                 binwidth = 5,
                 alpha = 0.7) +
  theme_bw()


# Barchart: geom_bar()
mpg %>% ggplot() +
  geom_bar(aes(x=class)) +
  theme_bw()

# Recall the stent example from Lecture 3?
# Let's convert Table 1.2 into a bar chart.
stents %>% ggplot() + geom_bar(aes(x = outcome30days))

# this only plots 0-30 days' outcomes
# how do we split this graph into treatment and control groups?
# we will add a layer called `facet_grid`

stents %>% ggplot() + 
  geom_bar(aes(x = outcome30days, fill = group)) +
  facet_grid(~group)