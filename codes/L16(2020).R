# Load packages
pacman::p_load(tidyverse, data.table, broom, kableExtra)
set.seed(8)

# One sample t-test
# A version of textbook example:
# Whether a typical US runner is getting faster or slower
# The average time for all runners who finished the Cherry Blossom Race
# in 2009 was 94.25
# We want to determine using data from 1000 randomly chosen participants
# in the 2017 Cherry Blossom Race whether runners in this 
# race are getting faster or slower, versus
#the other possibility that there has been no change.
cbrace09 <- fread("data/CBrace09.csv")
cbrace17 <- fread("data/CBrace17.csv")

# the variable of our interest is `net_time`
# calculate the average time from `cbrace09` dataset
m_nettime09 <- mean(cbrace09$net_time, na.rm=T) 

# Draw a random sample of 1000 participants from 2017 race
cbrace17samp <- cbrace17 %>% slice(sample(1000))

mean(cbrace17samp$net_sec, na.rm=T) # this gets you the seconds

hist(cbrace17samp$net_sec/60, col = "steelblue",
     xlab = "Time (mins)", ylab = "Frequency")

samp_m_nettime <- mean(cbrace17samp$net_sec, na.rm=T)/60

samp_sd_nettime <- sd(cbrace17samp$net_sec, na.rm = T)/60

stderr_nettime <- samp_sd_nettime/sqrt(
   length(cbrace17samp$net_sec))

T_nettime <- (samp_m_nettime - m_nettime09)/stderr_nettime

1- pt(T_nettime, df=999, lower.tail = F)

t.test(cbrace17samp$net_sec/60, 
       mu=m_nettime09, 
       conf.level = 0.95, 
       alternative = 'less')

# Example: Take the variable `height` 
# from the `starwars` dataset
# and run a t-test to check whether
# height is different than zero
# we will make use of `t.test()`
x <- t.test(starwars$height, mu =0, conf.level = 0.999, 
            alternative = "two.sided")

broom::tidy(x)

# let's do this by hand
# step1: compute the average
height_ave <- mean(starwars$height, na.rm = T)

# step2: calculate the std deviation
height_sd <- sd(starwars$height, na.rm = T)

# step3: compute the standard error
# standard error = stdev/square root of sample size
SE_swht <- height_sd/(sqrt(nrow(starwars) - 
                             length(starwars$height
                                    [is.na(starwars$height)==T])))

# Assume that the pop mean is 175
# Now test whether height is different than 175
T_swht <- (height_ave - 175)/(SE_swht)

# in this example, the sample size is large enough.

# what if the sample size is small, ie,
# the normality assumption doesn't hold
x <- c(698,1104,1037,1889,1911,2416,2761,4382,1839,321)

xmean <- mean(x, na.rm=T)    # mean
xsd <- sd(x, na.rm = T)      # std deviation
nx <- length(x)              # sample size
stderr_x <- xsd/sqrt(nx)
df <- nx -1                 # degrees of freedom
T_x <- (xmean - 0)/stderr_x # t-stat
2 * pt(4.94, df = 9, lower.tail = FALSE) # p-value

# how do we compute the confidence interval?
# the first step - when sample size is small- is to
# figure out the critical t-stat beyond which
# you will reject the null
T_cr <- qt(p = 0.975, df = 9)

xmean + T_cr*stderr_x
xmean - T_cr*stderr_x


# let's do this again

x2      <-  c(6.3, 7.1, 5.8, 6.9)        # a random variable
nx2     <- length(x2)                    # number of observations 
mu_x2   <- mean(x2)                      # the average
ho      <- 7                             # The null hypothesis
SE_x2    <- sd(x2) / sqrt(nx2)           # The standard error
t_x2    <- ( mu_x2 - ho ) / SE_x2        # The t-statistic
P.z  <- 2*pt(t_x2, df = nx2 - 1, 
          lower.tail = TRUE)             # Get the p-value

2*(1 - pt(abs(t), df = n-1))


# Paired Sample t-test
# Example> textbooks dataset
# price difference
names(textbooks)
head(textbooks)

hist(textbooks$diff, col = "steelblue")
t.test(textbooks$diff, mu = 0, 
       conf.level = 0.95, 
       alternative = "two.sided")



# Hypothesis testing for difference in two means
df <- setDT(copy(ncbirths))
names(df)
head(df)

# we are interested in two variables: `weight` and `habit`
hist(df$weight[df$habit == "smoker"], 
     ylab = NULL, xlab = "Newborn Weight",
     main = "Mothers who smoked")
hist(df$weight[df$habit == "nonsmoker"],
     ylab = NULL, xlab = "Newborn Weight",
     main = "Mothers who did not smoke")

# difference in mean between the two groups
df2 <-
   df[is.na(habit)==F, .(sample_size = .N, 
                      mean_weight = mean(weight), 
                      sd_weight = sd(weight)), 
   by = 'habit']

# calculate std error
stderr_weight <- sqrt((df2$sd_weight[1]^2/df2$sample_size[1]) + 
                       df2$sd_weight[2]^2/df2$sample_size[2])

# calculate t-stat
T_weight <- (df2$mean_weight[1] - df2$mean_weight[2])/stderr_weight

# figure out the degrees of freedom
df_weight <- min(df2$sample_size[1], df2$sample_size[2])

# compute the p-value
2 * pt(T_weight, df = df_weight-1, lower.tail = FALSE)

# confidence interval computation
# mean difference (sample estimate)
meandiff_weight <- df2$mean_weight[1] - df2$mean_weight[2]
# upper limit
meandiff_weight + 1.96*stderr_weight
#lower limit
meandiff_weight - 1.96*stderr_weight

# all of this can be done using t.test()
tidy(t.test(weight~habit, data=df, conf.level=0.95))
