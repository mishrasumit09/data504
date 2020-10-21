pacman::p_load(tidyverse, data.table)
######################################
# In this code, we are going to cover:
# 0. how to load a dataset
# 1. select columns
# 2. create columns
# 3. rename columns
# 4. reorder columns
# 5. sort rows
# 6. subset rows
# 7. remove duplicates
# 8. summarize data
# 9. merge two datasets
######################################


# creating a data.table
# data.table is an enhanced version of data.frame
DT = data.table(
  ID = c("b","b","b","a","a","c"),
  a = 1:6,
  b = 7:12,
  c = 13:18
)

# reading a csv file from the internet
url <- "https://raw.githubusercontent.com/Rdatatable/data.table/master/vignettes/flights14.csv"
flights <- fread(url)
flights

# look into the data
class(flights)
str(flights)


# `data.table` is structured thus:
# `DATAFRAME[ROWS,COLUMNS]`


# 1. selecting a/more than one column:
flights[,year]
ans <- flights[,year]

# we are going to select four columns:
#  year, month, day, carrier
ans <- flights[, .(year,month,day,carrier)]
head(ans)

# selecting columns (an alternative method)
select_cols = c("arr_delay", "dep_delay")
flights[ , ..select_cols]

# drop a set of columns
ans <- flights[, !c("month","day","hour")]


# create a new column
df <- data.table(x = 1:6)
df2 <- df[, .(x,x2 = x^2)]
# creating many columns
# we are going to:
# square up x
# take cube of x
# take average of x 
df[, `:=`(x2 = x^2,
          x3 = x^3,
          x_m = mean(x))]


# 3. rename columns
# we are going to rename the column carrier as airlines
ans <- flights[, .(year, month, day, airlines = carrier)]


# 4. change the order of columns
# we want to put airlines as the first column
setcolorder(ans, c("airlines"))


# 5. sort data

db <- data.table(
  name = c("D", "Q", "M", "E"),
  score = c(20,16,18,15)
)

# we want to sort this data by name
db[order(name)]

# alternatively, you can use setorder()
# in this example, i have sorted the data
# in descending order of the column `name`
setorder(db, -name)

# 6. subset rows
# applying condition on data frame
ans <- flights[origin == "JFK" & month == 6L]

# extracting rows by index
ans <- flights[1:5]



# doing simple operations
# let's see how many trips had delay < 0
ans <- flights[, sum( (arr_delay + dep_delay) < 0 )]


# 7. remove duplicates
#let's create a fake data
movieDB <- data.table(
  director = c("Ozu", "Ozu", "Ritwik"),
  name =c("Tokyo Story", "Tokyo Story", "Meghe Dhaka Tara"),
  year = c(1953, 1953, 1960))

movieDB

unique(movieDB, by = "director")

# 8. summarize data
# select all flights with origin in JFK &
# select data for the month of June
# compute average arrival delay and departure delay
ans <- flights[origin == "JFK" & month == 6L,
               .(m_arr = mean(arr_delay), m_dep = mean(dep_delay))]

ans <- flights[origin == "JFK" & month == 6L, length(dest)]

# you can get the number of observations by
# using .N
ans <- flights[origin == "JFK" & month == 6L, .N]



# generating aggregates
ans <- flights[, .(.N), by = .(origin)]
ans

# or 
ans <- flights[, .N, by = origin]

# example 2
ans <- flights[carrier == "AA", .N, by = origin]
ans

# example 3
ans <- flights[carrier == "AA", .N, by = .(origin, dest)]
head(ans)


# example 4
ans <- flights[carrier == "AA",
               .(mean(arr_delay), mean(dep_delay)),
               by = .(origin, dest, month)]
ans


#  how many flights started late but 
# arrived early (or on time), started and arrived late
ans <- flights[, .N, .(dep_delay>0, arr_delay>0)]
ans


# 9. merge two datasets

# we create two variables x and y
x <- c("x1", "x2", "x3", "x4", "x5")
y <- c("y1","y2","y3","y4")

# we create two data frames DT1 and DT2
# both these datasets contain a common column (key) called
# ID 
DT1 <- data.table(ID = 1:5, x)
DT1
DT2 <- data.table(ID = c(2,4,6,8), y)
DT2

# merge two datasets
merge(DT1,DT2, by = "ID")
# note that this will output only matched rows

# what if you want all the rows from DT1 to be
# preserved in your output
merge(DT1,DT2, by = "ID", all.x = T)

# what if you want all the rows from DT2 to be stored in the merged file?
merge(DT1,DT2, by = "ID", all.y = T)

# okay, what about preserving all the rows from the two datasets?
merge(DT1, DT2, by = "ID", all = T)


# Further readings--
# check out
# https://atrebas.github.io/post/2019-03-03-datatable-dplyr/
