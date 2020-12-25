pacman::p_load(data.table, tidyverse, openintro,
               moderndive, broom, kableExtra,
               patchwork, infer)


# Build a `virtual_bowl`
bowl


# randomly draw balls
virtual_shovel <- bowl %>% 
  rep_sample_n(size = 25) %>%
  setDT()
virtual_shovel

virtual_shovel[, (.N/nrow(virtual_shovel)), by="color"]


# repeat this 1000 times
virtual_samples_25 <- bowl %>%
  rep_sample_n(size = 25, reps = 1000) %>%
  setDT()

# compute the propn of red balls
virtual_prop_red_25 <- 
  virtual_samples_25[,.(prop.red = sum(color=="red")/25) , 
                     by = "replicate"]

################################################

# randomly 100 balls
virtual_samples_100 <- bowl %>%
  rep_sample_n(size = 100, reps = 1000) %>%
  setDT()

virtual_prop_red_100 <- 
  virtual_samples_100[,.(prop.red = sum(color=="red")/100) , 
                     by = "replicate"]


# randomly draw 400 balls
virtual_samples_400 <- bowl %>%
  rep_sample_n(size = 400, reps = 1000) %>%
  setDT()


virtual_prop_red_400 <- 
  virtual_samples_400[,
                      .(prop.red = sum(color=="red")/400) , 
                      by = "replicate"]


p1 <- 
  ggplot(virtual_prop_red_25, 
       aes(x = prop.red)) +
  geom_histogram(binwidth = 0.05, 
                 boundary = 0.4, 
                 color = "white",
                 fill = "steelblue") +
  labs(x = "Proportion of 25 balls that were red", title = "25")


p2 <- 
  ggplot(virtual_prop_red_100, 
         aes(x = prop.red)) +
  geom_histogram(binwidth = 0.05, 
                 boundary = 0.4, 
                 color = "white",
                 fill = "steelblue") +
  labs(x = "Proportion of 100 balls that were red", title = "100")


p3 <- 
  ggplot(virtual_prop_red_400, 
         aes(x = prop.red)) +
  geom_histogram(binwidth = 0.05, 
                 boundary = 0.4, 
                 color = "white",
                 fill = "steelblue") +
  labs(x = "Proportion of 400 balls that were red", title = "400")


# plot the three distributions together
p1 + p2 + p3


# now, compute the mean and SD for the three distributions
mean_sd <- function(x){
  return(c(Average = round(mean(x, na.rm = T),3),
           StDev = round(sd(x, na.rm = T),3)))
}

sapply(list(samp25  =virtual_prop_red_25$prop.red, 
            samp100 =virtual_prop_red_100$prop.red, 
            samp400 =virtual_prop_red_400$prop.red),
       mean_sd) -> msdred
bowl %>%
  setDT() %>% 
  .[, .(prop.red = sum(color=="red")/nrow(bowl))] -> mean_red


# compute the confidence interval
ciFun <- function(x) {
  return(c(ci.lower = round(mean_red - 1.96*sd(x),2),
           ci.upper = round(mean_red + 1.96*sd(x),2)))
}

sapply(list(samp25  =virtual_prop_red_25$prop.red, 
            samp100 =virtual_prop_red_100$prop.red,
            samp400 =virtual_prop_red_400$prop.red), 
       ciFun) %>% data.frame() -> ci_red
ci_red %>%
  setDT() %>%
  kable() %>%
  kable_minimal() %>%
  column_spec(1:3,bold=T, color = c("red", "blue"))


# CI the infer way

bowl_sample50 = slice(bowl, sample(50))

bowl_sample50 %>%
  specify(response = color,
          # specify response variable
          success = "red"
          # define red as success
  ) %>%
  # bootstrap
  generate(reps = 1000, type = "bootstrap") %>%
  # compute proportion
  calculate(stat = "prop") -> prop_redbs

# compute CI
percentile_ci_1 <- prop_redbs %>%
  get_confidence_interval(level = 0.95, type = "percentile")
percentile_ci_1

# visualize the data
prop_redbs %>%
  visualize(bins = 15) +
  shade_confidence_interval(endpoints = percentile_ci_1) +
  geom_vline(xintercept = 0.375, linetype = "dashed")


# Hypothesis testing for proportion
df <- fread("data/bm_clean.csv")
table(df$race, df$call) %>% addmargins()

prop.table(table(df$race, df$call)) %>% 
  addmargins() %>%
  round(2)


# hypothesis testing
# define a function for simulation
set.seed(3524)
rand.est.prop <- function() {
  treatment <- sample(df$race)
  return(mean((df$call=="Yes")[treatment== "Black"]) - 
           mean((df$call=="Yes")[treatment == "White"]))
}

#Sampling distribution of the difference
distribution.call <- replicate(1000, rand.est.prop())
distribution.call

hist(distribution.call, breaks = 200, main = NULL,
     xlab = NULL, ylab = NULL)
abline(v=-0.02, 
       lwd = 3, col='red')


# Let's go back to the bowl example
set.seed(3125)
sampledBowl <- slice(bowl, sample(50))
pRedSamp <- mean(sampledBowl$color=="red")

# the actual mean is 0.375
# the sample mean is 0.3
# let's see how different these two are.
# we need the following:
# 1. a test statistic
# 2. p value
seRedSamp <- sqrt((0.375*0.625)/50)

zRedSamp <- round((0.3 - 0.375)/seRedSamp,2)
print(zRedSamp)

# the test-statistic is 1.1 SD away from the mean

# let's compute the p-value
pnorm(zRedSamp, mean = 0, sd = 1, lower.tail = TRUE)
