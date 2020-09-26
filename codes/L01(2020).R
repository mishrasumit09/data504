# intro to data.table
# writing loops and functions in R

pacman::p_load(tidyverse, dplyr, data.table, 
               microbenchmark,wooldridge)

# learn data.table

# dplyr versus data.table

# code with dplyr
starwars %>%
  filter(species == "Human") %>% 
  group_by(homeworld) %>% 
  summarise(mean_height = mean(height))

# code with data.table()
starwars_dt = as.data.table(starwars)
starwars_dt[species == "Human", 
            mean(height, na.rm=T), 
            by=homeworld]


# order/arrange data with data.table()
setorder(starwars_dt, 
         birth_year, 
         na.last = TRUE)
head(starwars_dt[, name:birth_year]) 


# manipulate column using :=
wage1.new <- wage1 %>%
  select(wage, exper, educ, female, tenure, nonwhite) %>%
  as.data.table()
wage1.new[, lwage := log(wage)]
head(wage1.new)

# creating multiple columns
# example 1
dt <- data.table(mtcars)[,.(mpg, cyl)]
dt[,`:=`(avg=mean(mpg), med=median(mpg), min=min(mpg)), by=cyl]
head(dt)
# example 2
wage1.new[, `:=`(lwage = log(wage), 
                 expersq = exper*exper, 
                 tenuresq = tenure*tenure)]
head(wage1.new)

# Sub-assign by reference
# create a fake data
DT2 <- data.table(a = -2:2, b = LETTERS[1:5])
DT2 
# change b to NA wherever a < 0
DT2[a < 0, b := NA][] 


# Chaining multiple operations
# adding a new column and their transformation(s)
DT = data.table(x = 1:2)
DT[, z := 5:6][, z_sq := z^2][]

DT %>%
  .[, xz := x+z] %>%
  .[, xz_sq := xz^2] %>%
  .[]

# Remove a column
DT[, xz_sq := NULL]
DT

# Subset by column
# subset by column position
names(starwars_dt)
starwars_dt[, c(1:3, 10)] %>% head(2)
# subset by column name
starwars_dt[, .(name, height, mass, homeworld)] %>% head(2)

# exclude a column through negation
starwars_dt[, !c("name", "height")]

#setnames[starwars_dt, old = "name", new = "alias"][]
#setnames[starwars_dt, old = "alias", new = "name"]

#
wage1.new[, mean(wage, na.rm=T)] #computes mean

# create a new column for average wage
wage1.new[, mean_wage := mean(wage, na.rm=T)] %>% 
  head(5)

# get total number of observations
wage1.new[, .N]

# Group by
starwars_dt[, mean(height, na.rm=T), 
            by = species] #Collapse by single variable
starwars_dt[, .(species_height = mean(height, na.rm=T)), 
            by = species]:#name the summary variable
starwars_dt[, mean(mass, na.rm=T), 
              by = height>190] #Conditionals work too.
starwars_dt[, species_n := .N, by = species][] 

#Add an aggregated column to the data
wage1.new[, mean(wage, na.rm=T),
          by = female] # collapse by a single variable
wage1.new[, .(gender_wage = mean(wage, na.rm = T)),
          by = female] %>% head(5) # name the summary variable
wage1.new[, .(gender_wage = mean(wage, na.rm=T)), 
          by = educ > 11] # conditional works as well!
wage1.new[, .(gr_wage = mean(wage, na.rm = T)),
          by = .(female, nonwhite)][]

# Efficient subsetting with .SD
wage1.new[, .(mean(wage, na.rm = T), 
              mean(exper, na.rm = T), 
              mean(educ, na.rm = T)),
          by = female]
# using lapply to summarize
wage1.new[, 
          lapply(.SD, mean, na.rm=T),
          by = female, 
          .SDcols = c("wage", "educ", "exper")] %>% 
  head(2) ## Just keep everything on the slide

# what if we want to summarize all the variables in the dataset?
DT
DT[, lapply(.SD, mean)]


# Using keyby
# setting a key
DT = data.table(x = 1:10, y = LETTERS[1:10], key = "x")
# you can have multiple keys
DT = as.data.table(DT, key = c("x", "y"))
DT

# First create a keyed version of the storms data.table.
## Note that key variables match the 'by' grouping variables below.
storms_dt_key = as.data.table(storms, 
                              key = c("name", "year", "month", "day"))
## Collapse function for this keyed data.table. 
## Everything else stays the same.
collapse_dt_key = function() {
  storms_dt_key[, .(wind = mean(wind), 
                    pressure = mean(pressure), 
                    category = first(category)), 
                by = .(name, year, month, day)]
}

# Merge datasets
dataset1 <- as.data.table(data.table(
  name = c("Arun", "Nishi", "Preeti"),
  age = c(24, 22, 24)),
  key = name)
dataset2 <- as.data.table(
  data.table(name = c("Arun", "Nishi"),
             age = c(24, 22),
             major =c("Commerce", "Maths")),
  key = name)
dataset1[dataset2, on = "name"]

merge(dataset1, dataset2, by = "name")

## left join
merge(
  dataset1, 
  dataset2, 
  all.x = TRUE, ## omit for inner join
  by = "name")

## note that age variable appears twice. 
## let's try to fix that
merged <- merge(
  dataset1, 
  dataset2, 
  all.x = TRUE, ## omit for inner join
  by = "name")
colnames(merged) <- gsub('.x','',names(merged))
merged[, age.y := NULL]


# Reshaping data with data.table()
## dcast(): convert wide data to long data
## melt(): convert long data to wide data
## tidyfast functions
### tidyfast::dt_pivot_longer(): wide to long
### tidyfast::dt_pivot_wider(): long to wide

## wide to long--
stocks = data.table(time = as.Date('2020-01-01') + 0:10,
                    X = rnorm(11, 0, 1),
                    Y = rnorm(11, 0, 2),
                    Z = rnorm(11, 0, 4))
melt(stocks, id.vars ="time")
## some more cleanup
stocks_long = melt(stocks, id.vars ="time", 
                   variable.name = "stock", value.name = "price")

## long to wide
dcast(stocks_long, 
      time ~ stock, 
      value.var = "price")


## library(ggplot2) # already loaded
storms_dt <- as.data.table(storms)
storms_dt[, .(wind = mean(wind), 
              pressure = mean(pressure), 
              category = first(category)), 
          by = .(name, year, month, day)] %>%
  ggplot(aes(x = pressure, y = wind, col=category)) +
  geom_point(alpha = 0.3) + 
  theme_minimal()
