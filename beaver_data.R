?beaver2
str(beaver2)

beaver2_data <- beaver2

summary(beaver2_data)

# hint - use pairs() function
pairs(beaver2_data)

# Question 1
# Research Question:
# Does activity has any effect on beaver body temperature?

# H0: Beaver body temperature is not effected by activity
# H1: Beaver body temperature is effected by activity

# look at the correlation
# between the variables
pairs(beaver2_data, 
      labels = colnames(beaver2_data), 
      main = "Beavers dataset correlation plot")

# examining body temp and activity
# so the vars need converted first
beaver2_data$activity <- factor(beaver2_data$activ, 
                                labels = c("no", "yes"))

# Analytics correlation of activity
plot(beaver2_data$activity, beaver2_data$temp)

library("lattice")

histogram(~ beaver2_data$temp | beaver2_data$activity,
          data = beaver2_data, 
          main = "Distribution of beaver activity data", 
          xlab = "Temp (degrees)", 
          ylab = "Activity")

tapply(beaver2_data$temp, beaver2_data$activity, median)

# Q-Q plot for temp
qqnorm(beaver2_data$temp)
qqline(beaver2_data$temp, 
       col = "red")

# Q-Q plor for activity
with(beaver2_data, {qqnorm(temp[activity == "no"], 
                           main = "Inactive data") 
  qqline(temp[activity == "no"])})

with(beaver2_data, {qqnorm(temp[activity == "yes"], 
                           main = "Inactive data") 
  qqline(temp[activity == "yes"])})

normality_test <- with(beaver2_data, tapply(temp, activity, shapiro.test))
normality_test
