# One sample t-test
x <- t.test(starwars$height, mu =0, conf.level = 0.999, alternative = "two.sided")

broom::tidy(x)


height_ave <- mean(starwars$height, na.rm = T)
height_sd <- sd(starwars$height, na.rm = T)

SE_swht <- height_sd/(sqrt(nrow(starwars) - 
                             length(starwars$height
                                    [is.na(starwars$height)==T])))

# pop mean is 175
T_swht <- (height_ave - 175)/(SE_swht)


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
