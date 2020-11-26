# Code for chapter 4 (lecture 9 and 10)
pacman::p_load(tidyverse, data.table, readxl, openintro)


# generate normal distribution
x1 <- rnorm(1000, mean = 0, sd = 1)


# plot normal distribution
p <- 
  ggplot(data = data.frame(x = rnorm(1000, mean = 0, sd = 1)), 
            aes(x)) 
p <- p + stat_function(fun = dnorm, args = list(mean = 0, sd = 1), 
                       col = "steelblue")
p <- p + theme_bw()
print(p)

# plot normal distribution
p1 <- ggplot(data = data.frame(x = c(-4, 4)), aes(x)) +
  stat_function(fun = dnorm, n = 101, 
                args = list(mean = 0, sd = 1),
                col = "steelblue", lwd = 1.2) + 
  ylab("") +
  scale_y_continuous(breaks = NULL) + 
  theme_minimal()
p1


# calculate percentile
pnorm(1800, mean = 1500, sd = 300)
pnorm(24, mean = 21, sd = 5)


# Heinz example
pnorm(35.8, mean = 36, sd = 0.11)
pnorm(36.2, mean = 36, sd = 0.11)



# finding cutoff (body temperature)
z_bt <- qnorm(0.03) # gets you the z-score
mean_bt <- 98.2 # mean body temperature
sd_bt <- 0.73   # st dev

# z_bt = (obs_bt - mean_bt)/sd_bt
# obs_bt = z_bt*sd_bt + mean_bt
obs_bt <- z_bt*sd_bt + mean_bt

# finding cutoff-2 (highest 10%)
z_bt2 <- qnorm(0.9)
obs_bt2 <- z_bt2*sd_bt + mean_bt


# choose function in R
choose(9,2)


# birthday problem
alldays <- seq(1, 365, 1)
sameday <- 0
sims <- 1000
people <- 24

for (i in 1:sims) {
  room <- sample(alldays, people, replace = TRUE) 
  if (length(unique(room)) < people) 
    sameday <- sameday+1
}
cat("Probability >=2:", sameday/sims, "\\n")


