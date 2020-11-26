# Code for chapter 3 (lecture 7 and 8)
pacman::p_load(tidyverse, data.table, readxl, openintro)

# toss a coin in R
set.seed(1L)
coin <- c("head", "tail") # generate a coin in R
sample(coin, size=1, rep=T) # flip the coin once
sample(coin, size=100, rep=T) # flip the coin 100 times
tosses <- sample(coin, size=100, rep=T)
table(tosses) %>% addmargins()  # frequency table

# roll a die in R
die <- 1L:6L # generate a die
sample(die, size=1, rep=T)
sample(die, size = 100, rep=T)
roll.die <- sample(die, size=100, rep=T) 
table(roll.die) %>% addmargins() # frequency tables

# practice problem 1 (class)
# CV callback rate
df <- fread("./data/bm_clean.csv")
names(df)

# we want to compute the probability that
# a randomly drawn CV from this data is
# either a black person's or a woman's
prac1 <- table(df$race, df$gender) %>% addmargins()

# step 1: calculate the probability (race = black)
pr_black <- prac1[1,3]/prac1[3,3]
# step 2: calculate the probability (gender = woman)
pr_woman <- prac1[3,2]/prac1[3,3]
# step 3: compute the probability (race = black AND gender = woman)
pr_bw <- prac1[1,2]/prac1[3,3]
# step 4: use the rule to calculate the probability 
# (race = black)
pr_bOw <- round(pr_black + pr_woman - pr_bw, 2)


# conditional probability
photo <- setDT(copy(photo_classify))
table(photo$mach_learn, photo$truth) %>% 
  addmargins() # produces the contingency table

prop.table(table(photo$mach_learn, photo$truth)) %>% 
  addmargins() %>% 
  round(2) # marginal and joint probabilities 


# probability of an ace followed by 3
cards = c(2:10, "J", "Q", "K", "A")
suits = c("♠", "♥", "♦", "♣")
deck <- paste0(rep(cards, length(suits)),  #card values
               rep(suits, each = length(cards))) #suits
# compute probability that you get an ace in the first shuffle
prob.ace1 <- round(length(deck[deck %like% 'A'])/length(deck),2)
# now, we have an ace out of our deck.
# we shall compute the probability of getting a three
prob.three2 <- round(length(deck[deck %like% '3'])/(length(deck)-1),2)

# calculate the prob:
# ace in first round AND three in second round
prob12 <- prob.ace1*prob.three2


# tree diagram

treeDiag(c('Midterm', 'Final'),
         c(0.13,0.87),            # probability for the main branch
         list(c(0.47, 0.53),      # conditional probabilities
              c(0.11, 0.89)),
         textwd = 0.2,            # width for text 
         solwd = 0.35,            # width for the solution
         cex.main = 1.4,          # size of title
         c('A', 'Other'),         # primary branch text
         c('A', 'Other'),         # secondary branch text
         digits = 2,              # decimal places
         col.main = "purple",     # color for title
         showWork = TRUE)         # show the computation



# continuous distribution


# card game
win = c(rep(10, 12), rep(50, 4), rep(100, 1), rep(0, 35))
win = factor(win, levels = c(0,10,50,100))
barplot(table(win)/52, col = "steelblue", las = 1)

# calculate variance for the card game
cardgame <- data.table(win)
cgsummary <- cardgame[, .(prob = .N/nrow(df)), by = "win"]
cgsummary[, win := as.numeric(as.character(win))] %>% 
  .[, `:=`(xpx = win*prob, 
           xmxsq = (win - mean(win))^2)] %>% 
  .[, varx := prob*xmxsq]
varCardGame <- sum(cgsummary$varx)
sdCardGame <- sqrt(varCardGame)
