# Basic Regression
# Moderndive: Chapter 5

pacman::p_load(tidyverse, data.table, openintro,
               broom, kableExtra, 
               moderndive, skimr, haven, wooldridge)

# Outcome: teaching score
# Predictor: beauty score
evals_ch5 <- evals[,c("ID", "score", "bty_avg", "age")]
head(evals_ch5)

# step 1: generate summary stats
glimpse(evals_ch5)

evals_ch5 %>% select(score, bty_avg) %>% skim()

# step 2: check correlation
cor(evals_ch5$score, evals_ch5$bty_avg)

# step 3: visualization
ggplot(evals_ch5, aes(x = bty_avg, y = score)) +
  geom_point() +
  labs(x = "Beauty Score",
       y = "Teaching Score",
       title = "teaching and beauty scores")

# add a regression line
ggplot(evals_ch5, aes(x = bty_avg, y = score)) +
  geom_point(color = "steelblue") +
  labs(x = "Beauty Score", y = "Teaching Score",
       title = "Relationship between teaching and beauty scores") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme_bw()

# step 4: run OLS
score_model <- lm(score ~ bty_avg, data = evals_ch5)
tidy(score_model)
get_regression_table(score_model)

# get predicted values and residuals
regression_points_sm <- get_regression_points(score_model)


# Example 2
# Relationship between the propn of disadvantaged groups (SC-ST)
# and the propn of households without any assets
assets <- read_dta("data/AssetsCensus11.dta")

# step1: inspect the data
head(assets)
glimpse(assets)

# Outcome: pNoAssets
# Predictor: pSCST

assets %>% select(pNoAssets, pSCST) %>% skim()


# step 2: check correlation
cor(assets$pNoAssets, assets$pSCST)

# step 3: visualization
ggplot(assets, aes(x = pSCST, y = pNoAssets)) +
  geom_point(col = "steelblue") +
  labs(x = "Propn SC-ST in a District",
       y = "Propn without any assets",
       title = "")+
  theme_bw()

# step 4: add a regression line
# add a regression line
ggplot(assets, aes(x = pSCST, y = pNoAssets)) +
  geom_point(color = "steelblue") +
  labs(x = "Propn SC-ST in a District",
       y = "Propn without any assets",
       title = "")+
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme_bw()

# step 4: run OLS
assets_model <- lm(pNoAssets ~ pSCST, data = assets)
tidy(assets_model)
get_regression_table(assets_model)


# get predicted values and residuals
regression_points_am <- get_regression_points(assets_model)


# Example 3
# Outcome: wage
# Predictor: gender

wageDta <- setDT(copy(wage1))

head(wageDta)

# columns that we are interested in:
# `lwage`: wage
# `female`: = 1 if woman, 0 if man
tidy(lm(wage~factor(female), data=wageDta))


# Example 4
# Outcome: `total_pr`
# Predictor: `cond`
mariokart <- setDT(copy(mariokart))
head(mariokart)

tidy(lm(total_pr ~ factor(cond), data = mariokart))
