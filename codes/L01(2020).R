# In order to work on anything in R, you need to have packages.
# This is analagous to buying a phone, and installing apps from
# play store
install.packages("pacman")
pacman::p_load(tidyverse,data.table, here)

# In R, we work with objects 
# Objects are of types- numeric, character, logical, and factor
# (in excel, your work revolves around cells)
# creating an object in R

25

# if you want R to remember that 25 is the minimum age
# for drinking in Maharashrtra, you need to save it as
# a named object.
min_age <- 25

# note that the LHS is the name
#      and the RHS contains the values.
# the two are separated by `<-` which translates to equal
# this will be our go to syntax. 


# Different types of objects--

# 1. Numeric
a_number <- 99
an_integer <- 99L

set_integers <- 1L:100L

# 2. Character
an_alphabet <- "e"

# 3. Logical
# Did Rashiben remove chana from cooker?
that_is <- TRUE


#---------------------------------------------------------------------#
# Rules for creating objects in R:
# • R is case-sensitive.
# • Names cannot contain any special character 
#   except for underscore or period.
# • R overwrites objects.
# • The list of objects can be gleaned by typing ls() in the console
#---------------------------------------------------------------------#


# Functions in R
# performing any task in R requires the knowledge of functions.
# you need to call the name of the function 
# and sandwich the operation (formally known as the argument) 
# within parentheses.
# `NAME OF THE FUNCTION(YOUR ARGUMENTS GO HERE)`

# Basic functions in R:
#   i. print(): print an object
#  ii. mean():  calculates average
# iii. round(): rounds off any number
#  iv. sample(): randomly sample objects.
# Let's run one by one each of these functions

print(min_age)

print(a_number)

mean(set_integers)

round(2.8584)

# Each function can have one or many arguments 
# For example, when we rounded off 2.8584 in R, we got 3 as the answer.
# What if we wanted R to round off this number upto 2 decimal places?
# We will add another argument to the function.
# Fine, but how do I know what argument to add?
# we will invoke args() to get all the arguments of the function round()
args(round)

# ah, so we see that round has two arguments:
#           1. x (a number or a set of numbers)
#           2. digits (probably, the number of decimal places?)
round(2.8584, digits=2)


sample(set_integers) # scrambles the numbers

# we want to pick five numbers from `set_integers`.
# how do we do that?
# check arguments of this function!
sample(set_integers,size=5, replace = T)


# When you create an object in R , 
# you should try to know more about the object. 
# One way is to look at the
# Global Environment window of R Studio 
# which contains the following information
# Name, Type, Length, Size, Value.
# The structured way of doing this:
# use functions!
#  class(): tells you whether the object is numeric/character/logical
#  str(): quick snapshot of the object
#  typeof(): what's the specific type within the class?

class(set_integers)
class(a_number)
class(an_alphabet)

str(set_integers)
str(a_number)

typeof(set_integers)
typeof(a_number)



# a key object in R is a vector 
# vector are like individual columns in excel.
# the header is the name of the object in R.
# we put comma separated objects inside c() to create vectors
nv <- c(10,30,60,90) # numeric
cv <- c("A", "B", "C", "D") # character
lv <- c(T, T, F, F) #logical

# Index in R
# Just like excel has A1, B25, etc, we have index in R.
# We can call any object by appending [] to the object.
nv[1]
nv[2]
nv[1:2]
nv[2:4]

# EXERCISE: 
# what if i want the first and the fourth object of nv?


# Some useful functions to create vectors:
# rep(), seq()
nv.rep <- rep(nv, 2)

ap <- seq(from=1, to=9, by=2)

# Working with logicals
# You have data on a student's age, and you want to see if
# their age is above the minimum drinking age object
# you defined earlier.
stu_age <- 24
stu_age >= min_age # greater than or equal to
stu_age == min_age # equal to
stu_age != min_age # not equal to


# University tenures are dependent on 
# number of papers or student evaluation
num_papers <- 3 #number of papers
teach_rating <- 4.2 #average rating
num_papers >= 4 | teach_rating >= 4 #check if eligible for promotion

# you can check if a vector contains a given set of objects by
# using `%in%`
16 %in% nv

c(10,15,20) %in% nv


# More logical functions:
# any(): check if any of the objects meet the condition
# all(): check if all the objects meet the condition
# which(): tells you which object satisfies the condition

grp_stu <- c(20,23,26,29,24)
any(grp_stu >= 25)
all(grp_stu >=25)
which(grp_stu >= 25)

# Filtering data or subsetting data
# we will use [] to subset vectors
set_integers <- 1:100

set_integers[set_integers > 20]
set_integers[set_integers < 25]

# Multiple conditions can also be added
# There are two types of multiple conditions:
# and (denoted by: `&`) and or (represented by: `|`)

set_integers[set_integers > 20 & set_integers < 50]
set_integers[set_integers < 20 | set_integers >= 80]

set_integers[set_integers > 100]


# summarizing a vector in R
class(set_integers)
length(set_integers) # returns the length of the object
max(set_integers) # tells you the max
min(set_integers) # tells us the min
sum(set_integers) # computes the sum
var(set_integers) # calculates the variance
quantile(set_integers, probs = seq(0, 1, 0.1)) #percentiles
summary(set_integers)

exam_scores <- c(80,45,72,90)
names <- c("Chin2", "Pin2", "Min2", "Rin2")


# how do we stitch these different objects together?
# enter data.frame
exam_db <- data.frame(names, exam_scores)


# let's read an excel file into R
df <- readxl::read_excel("data/wbGDPdata.xlsx")

# read a csv file in R
df <- read_csv("data/schoolDB.csv")
