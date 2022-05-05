# Fuel efficiency and car weight

cars <- mtcars
str(cars)

# Check for missing data
incomplete_data <- cars[!complete.cases(cars), ]
nrow(incomplete_data)

install.packages("psych")
library(psych)
pairs.panels(cars,
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

# examining does car weight have an effect on mpg?
# Vars are wt, mpg

# Compare car weight and mpg
plot(cars$wt, 
     cars$mpg, 
     col = "blue", 
     main = "Comparision of car weight with mpg", 
     xlab = "Weight (lbs)", 
     ylab = "Mpg")

# Q-Q plot of the relationship between both variables

# Display Q-Q plot for weight
with(cars, {qqnorm(wt, 
                  main = "Normal Q-Q plot of weight data", 
                  xlab = "Weight (1000 lbs)")
     qqline(wt, col = "blue")})

# Display Q-Q plot for mpg
with(cars, {qqnorm(mpg, 
                   main = "Normal Q-Q plot of mpg data", 
                   xlab = "Mpg")
  qqline(mpg, col = "blue")})

# Formal test for normality
# using the shapiro-wilk test
normality_test_wt <- shapiro.test(cars$wt)
normality_test_wt$p.value

normality_test_mpg <- shapiro.test(cars$mpg)
normality_test_mpg$p.value

cor.test(cars$wt, cars$mpg, method = "pearson")


# wrong test
wilcox.test(cars$wt~cars$mpg)
