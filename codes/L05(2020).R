#' 
#' Chapter 02: Open Intro
#' Summarizing Data
#' 

# Packages required for this week's lectures:
pacman::p_load(tidyverse, data.table, readxl, janitor, openintro)

# Scatter plot:
# plot miles per gallon (hwy) on y axis versus
#      displacement (displ) on x axis
ggplot(mpg, aes(displ, hwy, colour = class)) + 
  geom_point(size = 3) + theme_bw()


# Dot plot
df <- setDT(openintro::gpa)
df %>% ggplot(aes(x = gpa)) + 
  geom_dotplot(fill = "deepskyblue") +
  theme_minimal() +
  theme(axis.line.x = element_line())


ggplot(data.frame(x =df$gpa[df$gpa <=4], y = 1), aes(x, y)) +
  geom_point(size = 6, color = "purple", alpha = 0.5) +
  coord_cartesian(ylim = c(0,6), clip = "off") +
  labs(y = NULL) + 
  guides(y = "none") + 
  theme_minimal(base_size = 16) + 
  scale_x_continuous(breaks = c(2,2.5,3, 3.5, 4),
                     name = "GPA") + 
  theme(axis.line.x = element_line(),
        panel.grid = element_blank())


# Dot plot with mean
ggplot(data.frame(x =df$gpa[df$gpa <=4], y = 1), aes(x, y)) +
  geom_point(size = 6, color = "purple", alpha = 0.5) +
  geom_point(aes(x = mean(df$gpa), y = 0), size = 8, shape = 17,
             color = "red") + 
  coord_cartesian(ylim = c(0,6), clip = "off") +
  labs(y = NULL) + 
  guides(y = "none") + 
  theme_minimal(base_size = 16) + 
  scale_x_continuous(breaks = c(2,2.5,3, 3.5, 4),
                     name = "GPA") + 
  theme(axis.line.x = element_line(),
        panel.grid = element_blank())


# Exercise: Plot interest rate from loan50 dataset


# Histogram
hist(loan50$interest_rate, col = "steelblue", 
     main = NULL, xlab = "Interest Rate") 


# compute variance manually
# starwars: height
stw <- setDT(copy(starwars))
swh <- stw[is.na(height)==F, .(height)]
swh[, `:=`(term1 = (height - mean(height))^2, 
           term2 = (length(height) - 1))]
unique(swh[, .(var.height = sum(term1)/term2)])

sd.height <- sd(starwars$height, na.rm = T)

unique(swh[, .(sd.height = sqrt(sum(term1)/term2))])


# manually compute the median of the variable height:
height <- starwars$height
median(height) #canned function

htsorted <- sort(height)
median.ht <- (htsorted[(length(htsorted) - 1)/2] + 
              htsorted[(length(htsorted) + 1)/2])/2


# boxplot
height <- starwars$height[is.na(starwars$height)==F]
boxplot(height, col = "skyblue", ylab = "Height", axes = F, pch = 20)
axis(2)

# computing whiskers
iqr.ht <- IQR(height)
whisker1 <- quantile(height, probs = 0.75) + 1.5*iqr.ht
whisker2 <- quantile(height, probs = 0.25) - 1.5*iqr.ht

# extreme observations
set_num <- c(0, 0.5, 0.7, 0.9, 1.1, 1.5)
mean(set_num)
sd(set_num)
median(set_num)
IQR(set_num)

#replace 1.5 by 150
set_num[length(set_num)] = 150
mean(set_num)
sd(set_num)
median(set_num)
IQR(set_num)

# median and IQR are relatively less sensitive to extreme values
# for skewed distributions, it makes more sense to look at these
# two numbers.
# for symmetric distributions, mean and SD are also useful.


# for a skewed variable, it makes sense to transform the
# variable (most common transformation is log)
mass.sw <- starwars$mass[is.na(starwars$mass)==F]
hist(mass.sw) # very skewed distribution
hist(log(mass.sw)) # slightly better

# Categorical Data

# CV callback rate
df <- fread("./data/bm_clean.csv")
table(df$race, df$call) %>% addmargins()


# Bar plot (frequency)
df %>% 
  ggplot(aes(x = call)) + 
  geom_bar(fill = "steelblue") + 
  labs(y  = "Frequency") + 
  theme_bw()

# Bar plot (relative frequency)
df %>% 
  ggplot(aes(x = call)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), 
           fill = "steelblue") +
  labs(y = "Relative frequency") +
  scale_y_continuous(labels=scales::percent) +
  theme_bw()

# callback rates by race
df.t1 <- table(df$race, df$call) %>% addmargins()

bcr.t1 <- round((df.t1[1,2]/df.t1[1,3])*100,1)
bcr.t1

wcr.t1 <- round((df.t1[2,2]/df.t1[2,3])*100,1)
wcr.t1


# three different bar plots

# 1 - seg bar

df %>%
  ggplot(aes(x = race, fill = call)) +
  geom_bar() +
  labs(y = "Frequency",
       fill = "Callback") +
  theme_bw() +
  scale_fill_manual(values = c(COL[1], COL[1,3]), 
                    breaks = c("No", "Yes"))


# 2 - seg bar dodged

df %>%
  ggplot(aes(x = race, fill = call)) +
  geom_bar(position = "dodge") +
  labs(y = "Frequency",
       fill = "Callback") +
  theme_bw() +
  scale_fill_manual(values = c(COL[1], COL[1,3]), 
                    breaks = c("No", "Yes"))


# 3- race_rel_seg_bar

df %>%
  ggplot(aes(x = race, fill = call)) +
  geom_bar(position = "fill") +
  labs(y = "Relative frequency",
       fill = "Callback") +
  theme_bw() +
  scale_fill_manual(values = c(COL[1], COL[1,3]), 
                    breaks = c("No", "Yes"))


# side-by-side boxplot
boxPlot(mpg$hwy, fact = mpg$class, col ="skyblue", ylab = "")


# Introduction to hypothesis testing

fake.treatment <- sample(df$race) # shuffle the race variable
# create a new numeric variable for callback
df2 <- df[, callback := (call == "Yes")] 
# compute difference in callback rate using the shuffled race variable
new.diff <- round(mean(df2$callback[fake.treatment=="Black"]) -
  mean(df2$callback[fake.treatment=="White"]),1)

# how can we do this 10,000 times?
# the answer lies in constructing a function
# that takes the shuffled race variable and
# pops out the difference.

# detour: creating function in R
# let's say you want to create your own function for mean

myMean <- function(x) {
  x
  return(sum(x)/length(x))
}

set.seed(156)
myDiff <- function() {
  shuffled.race <- sample(df2$race)
  return(mean(df2$callback[shuffled.race=="Black"]) -
           mean(df2$callback[shuffled.race=="White"]))
}

# run this function one lakh times
distrib.diff <- replicate(100000, myDiff())
# plot the histogram for this distribution
hist(distrib.diff, col = "steelblue", ylab = NULL)
# place the actual difference on this graph as a vertical line
diff.callback <- 
  mean(df2$callback[df2$race=="Black"]) - 
  mean(df2$callback[df2$race=="White"])
abline(v = diff.callback, col = "red", lwd = 2)
