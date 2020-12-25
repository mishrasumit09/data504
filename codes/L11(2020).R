# Discrete distributions
# Binomial, Geometric, and Poisson Distributions
pacman::p_load(data.table, tidyverse, openintro,
               moderndive, broom, kableExtra,
               patchwork, infer)


set.seed(8)

rbinom(1,100,0.3) #generates numbers for you

binomData <- rbinom(1000, 100, 0.3) %>%
  as.data.frame()
names(binomData) <- c('data')

binomData %>% ggplot() + 
  geom_histogram(aes(x = data, 
                     y = stat(count / sum(count))), 
                 color = 'black',
                 fill  = 'steelblue')+
  geom_vline(xintercept = 30, 
             size = 1, 
             linetype = 'dashed',
             color = 'red') +
  theme_bw() +
  labs(x = 'Number of successes in 100 trials',
       y = 'Proportion',
       title = '1000 samples of Binomial(100, 0.3)')

dbinom(30, 100, 0.3)

binomData %>%
  ggplot() + 
  geom_histogram(aes(x = data,
                     y = stat(count/sum(count)),
                     fill = data == 30)) +
  theme_bw() + 
  theme(legend.position = 'none') +
  labs(x = 'Number of successes in 100 trials',
       y = 'Proportion',
       fill = NULL,
       title = '1000 samples of Binomial(100, 0.3)') +
  scale_fill_manual(values = c("steelblue", "orange")) 

binomData %>% setDT() %>%
  .[, .(propn=.N/nrow(binomData)), by = "data"]


# Cumulative probability 
# probability that a random binomial distribution
# takes a value less than or equal to a given value
pbinom(30, 100, 0.3)


binomData %>% 
  ggplot() + 
  geom_histogram(aes(x = data,
                     y = stat(count / sum(count)),
                     fill = data <= 30), 
                 color = 'black') +
  theme_bw() +
  theme(legend.position = 'none') +
  labs(x = 'Number of successes in 100 trials',
       y = 'Proportion',
       title = '1000 samples of b(100, 0.3)')+
  scale_fill_manual(values = c("steelblue", "orange")) 

binomData %>% setDT() %>%
  .[, .(propn=sum(binomData <= 30)/nrow(binomData))]

# Geometric Distribution
geomData <- rgeom(100, 0.5) %>%
  data.frame()
names(geomData) <- "data"

geomData %>%
  ggplot(aes(x=data,
             y=stat(count/sum(count))))+
  geom_bar(fill = "steelblue") +
  theme_bw() +
  labs(x = NULL,
       y = NULL)

# Poisson Distribution
# rpois(n, lambda):
# n: randomly generated numbers 
# lambda: average number of successes
set.seed(2)

poisData <- data.table('data' = rpois(1000, 10))

poisData %>% 
  ggplot() + 
  geom_histogram(aes(x = data, 
                     y = stat(count / sum(count))), 
                 fill = 'steelblue',
                 binwidth = 1) +
  geom_vline(xintercept = 10, 
             size = 1, 
             linetype = 'dashed',
             color = 'red') +
  theme_bw() +
  labs(x = 'Number of successes per period',
       y = 'Proportion',
       title = '1,000 samples of Pois(lambda = 10)')
