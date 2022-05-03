# 
normal_data <- read.csv("NormalData.csv")
non_normal_data <- read.csv("NonNormalData.csv")

opar <- par(no.readonly = TRUE)

par(mfrow = c(1,2))

# Create histogram to show distribution
# of both data frame
# to visally check for normality
hist(normal_data$x, 
     main = "Frequency chart of data distribution (parametric data)", 
     col = 'red')

hist(non_normal_data$x,
     main = "Frequency chart of data distribution (non-parematric data)", 
     col = 'red')

par(opar)

# Create a Q-Q plot for both data frames
# check if the points fall along the diagonal lines
qqnorm(normal_data$x, main = "Q-Q plot of first dataset variable")
qqline(normal_data$x)

qqnorm(non_normal_data$x, main = "Q-Q plot of second dataset variable")
qqline(non_normal_data$x)

# shapiro-wilk test
# if p-value > 0.05 then data variable is normally distributed
# if p-value < 0.05 then data variable is not normally distributed
# Test also present a W statistic; we'll use p-value
shapiro.test(normal_data$x)

# shapiro-wilk test for non-parametric data
shapiro.test(non_normal_data$x)

# Perform the Kolmogorov-Smirnov Test
# if p-value > 0.05 then data is normally distributed
# if p-value < 0.05 then data is not normally distributed
ks.test(normal_data$x, 'pnorm')
ks.test(non_normal_data$x, 'pnorm')

# Convert non-parametric data to parametric
# long transform
log_non_normal_data <- log10(non_normal_data$x)

# if badly skewed data then use this
neg_skewed_log_non_normal_data <- log10(max(non_normal_data$x + 1) - 
                                          non_normal_data$x)

qqnorm(log_non_normal_data)
qqline(log_non_normal_data)
qqnorm(neg_skewed_log_non_normal_data)
qqline(neg_skewed_log_non_normal_data)

hist(log_non_normal_data)
hist(neg_skewed_log_non_normal_data)

ks.test(log_non_normal_data, 'pnorm')
ks.test(neg_skewed_log_non_normal_data, 'pnorm')
