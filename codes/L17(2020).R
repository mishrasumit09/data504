# Lecture 17 Code
# Chapter 7: Inference for numeric variables
# ANOVA

pacman::p_load(tidyverse, data.table, kableExtra, car, openintro)

# Example 1: 
# Dataset> `mpg`
# Variables: `class` and `hwy`

mpgDT <- setDT(copy(mpg))[, .(hwy, class)] # keep the relevant columns

ggplot(mpgDT) +
  aes(x = class, y = hwy, color = class) +
  geom_jitter() +
  theme_bw() +
  theme(legend.position = "none")       # plot the data


# In ANOVA, we try to explain the difference 
# in a quantitative variable based on a qualitative variable. 
# So, here the question is whether different car classes 
# have different miles per gallon.
# H_0: no difference in average mileage across car-classes.
# H_A: at least one mean is different.

# We will first test the assumptions.

# Assumption 1: Normality

res_aov <- aov(hwy ~ class,
               data = mpgDT
)

par(mfrow = c(1, 2)) # combine plots

hist(res_aov$residuals, 
     main = NULL,
     xlab = "Residuals",
     col = "steelblue") # plot the histogram

# QQ-plot
qqPlot(res_aov$residuals,
       ylab = "Residuals",
       col = "pink",
       id = FALSE # id = FALSE to remove point identification
)

dev.off()

shapiro.test(res_aov$residuals) # formally test the assumption

# what does the test suggest? p-value is close to zero.


# Assumption 2: Equal Variances

boxplot(hwy~class,
        data = mpgDT,
        col = "steelblue")

leveneTest(hwy ~ class,
           data = mpgDT) # formal test

# what does the test result suggest?

# summary stats
mpgDT[, sapply(.SD, 
               function(x) list(mean=round(mean(x), 2), 
                                sd=round(sd(x), 2))
), by="class"] %>% 
  .[, .(mean_hwy = V1, sd_hwy = V2)]

# if the assumptions hold, you can run--
anova(lm(hwy~class, data = mpgDT))


# if the equal variance assumption doesn't hold, you should run--
oneway.test(hwy ~ class, 
            data = mpgDT,
            var.equal = F)

# if the normality assumption doesn't hold, you should run--
kruskal.test(hwy ~ class, data=mpgDT)


# Example 2

gpaDta <- data.table(Year = c(2009,2010,2011,2012,2013,2014),
                     GrpA = c(3.00, 3.34, 3.48, 3.63, 3.81, 3.20),
                     GrpB = c(2.45, 3.08, 3.32, 3.45, 2.70, 3.04),
                     GrpC = c(2.45, 2.76, 2.08, 3.36, 3.49, 2.70),
                     GrpD = c(2.34, 2.72, 2.04, 2.15, 2.51, 3.11))


gpaDta_long <- gpaDta %>%
  pivot_longer(cols = GrpA:GrpD, names_to = "Section", values_to = "GPA")

# perform ANOVA
anova(lm(GPA ~ Section, gpaDta_long))

# let's compute all of these quantities manually.
SSE.m <- sum((gpaDta_long$GPA - mean(gpaDta_long$GPA))^2 ) 
SSE <- sum( (gpaDta$GrpA - mean(gpaDta$GrpA))^2 +
              (gpaDta$GrpB - mean(gpaDta$GrpB))^2 +
              (gpaDta$GrpC - mean(gpaDta$GrpC))^2 + 
              (gpaDta$GrpD - mean(gpaDta$GrpD))^2 )  
SSR <- SSE.m - SSE
MSR <- SSR / 3   # (p - 1) or 3 degrees of freedom
MSE <- SSE / 20  # (n - p) or 20 degrees of freedom
Fval <- MSR/MSE
pVal  <- pf(Fval, 3, 20,lower.tail=FALSE)

# check whether assumptions are satisfied for this dataset


# Example 3

scoreA <- c(90,87,88,78, NA)
scoreB <- c(87,85,80, NA, NA)
scoreC <- c(95,93,90,88,85)

# create a dataframe 

scoreDta <- data.table(scoreA, scoreB, scoreC)

# we will compute the following
# – Degrees of freedom for Groups df_G
# – Sum of Squares between Groups SSG
# – Mean Square between Groups MSG
# – Degrees of freedom for Error df_E
# – Sum of Squared Error SSE
# – Mean Square Error MSE
# – Totals df_T and SST
# – F Statistic and p-value

# what's df_G?
# there are three groups
df_G <- ncol(scoreDta) - 1

# compute SSG.
m_scoreA <- mean(scoreDta$scoreA, na.rm = T)
m_scoreB <- mean(scoreDta$scoreB, na.rm = T)
m_scoreC <- mean(scoreDta$scoreC, na.rm = T)

m_scoreDta <- sum(scoreDta$scoreA, scoreDta$scoreB,
                  scoreDta$scoreC, na.rm = T)/(
                    ncol(scoreDta)*nrow(scoreDta) - 3)
SSG_scoreDta <- 
  length(scoreA[is.na(scoreA)==F])*(m_scoreA - m_scoreDta)^2 +
  length(scoreB[is.na(scoreB)==F])*(m_scoreB - m_scoreDta)^2 + 
  length(scoreC[is.na(scoreC)==F])*(m_scoreC - m_scoreDta)^2


# Compute MSG
MSG_scoreDta <- SSG_scoreDta/df_G

# Degrees of freedom for error
df_err <- nrow(scoreDta)*ncol(scoreDta) - 3 - ncol(scoreDta)

# Compute SSE
# Tedious way: compute difference between each individual
# cell and the group mean. square these differences and add up.
# Simple solution: compute within group variances
varScoreA <- var(scoreA, na.rm = T)
varScoreB <- var(scoreB, na.rm = T)
varScoreC <- var(scoreC, na.rm = T)

# SSE = sum of variances weighted by (length of vectors - 1)

SSE_scoreDta <- 
  (length(scoreA[is.na(scoreA)==F])-1)*varScoreA +
  (length(scoreB[is.na(scoreB)==F])-1)*varScoreB +
  (length(scoreC[is.na(scoreC)==F])-1)*varScoreC

# Compute MSE by dividing SSE by degrees of freedom for error
MSE_scoreDta <- SSE_scoreDta/df_err

# Total degrees of freedom
dfTot_scoreDta <- df_G + df_err

# The sum of squared total
SST_scoreDta <- SSG_scoreDta + SSE_scoreDta

# F-statistic
F_scoreDta <- MSG_scoreDta/MSE_scoreDta

# p-value
pf(q=F_scoreDta, df1=df_G, df2=df_err, lower.tail=FALSE)


# Example 4:
# Test Scores across teaching delivery types
scoreOnline <- c(72,84,77,80,81)
scoreHybrid <- c(83,73,84,81)
scoreFace2Face <- c(80,78,84,81,86,79,82)

# Follow the steps from example 3 to test
# the hypothesis that average scores are same
# across teaching mode



# Example 5
# Is batting performance related to player position in MLB?
# Variable Description--
# name: Player name
# team: The abbreviated name of the player’s team
# position: The player’s primary field position (OF, IF, C)
# AB: Number of opportunities at bat
# H: Number of hits
# HR: Number of home runs
# RBI: Number of runs batted in
# AVG: Batting average, which is equal to H/AB
# OBP: On-base percentage, which is roughly equal to the fraction
#      of times a player gets on base or hits a home run
bat18 <- setDT(copy(mlb_players_18)) %>%
  select(name, team, position, games, AB, H,
         HR, RBI, AVG, OBP)

# What's the null?  µ_OF = µ_IF = µ_C
bat18[, .(sample_size = .N, 
          mean_obp = mean(OBP), 
          sd_obp = sd(OBP)), by = "position"]


bat18subset <- bat18[AB >= 100]
bat18subset <- bat18[!position %in% c("P", "DH")]
pos <- list(c("LF", "CF", "RF"), c("1B", "2B", "3B", "SS"), "DH", "C")
POS <- c("OF", "IF", "DH", "C")

for (i in 1:length(pos)) {
  these <- which(bat18subset$position %in% pos[[i]])
  cat(length(these), "\n")
  bat18subset$position[these] <- POS[i]
}
bat18subset <- select(bat18subset, name, team,
                      position, AB, H, HR, RBI, AVG, OBP)
