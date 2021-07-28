# Youtube tutorial:
# https://www.youtube.com/watch?v=6_AF9mMuk9E&list=PLqzoL9-eJTNDdnKvep_YHIwk2AMqHhuJ0&index=5

# I can't find the data online. I will type it in

AIDSsurv <- data.frame(Time = c(2,3,6,6,7,10,15,15, 16, 27, 30, 32),
                        Event = c(1,0,1,1,1,0,1,1,1,1,1,1))

# 1 means that the observation ends because of death
# 0 means that the observation is censored (it ended by other cause)

# Load the survival package
library(survival)


# Building the Kaplan Meier (Product Limit) Model
km.model <- survfit(Surv(AIDSsurv$Time, AIDSsurv$Event) ~ 1,
                    stype = 1, ctype = 1)

# We use ~1 since we do not have any covariates (No X matrix). 
# If we do have X, we would have typed ~X

# stype = 1 and ctype = 1 is the default, it is the KM model

# Evaluating the results
km.model

# n is the number of sample
# events is the number of recorded events (1s in our data)
# median is the median survival time. The probability to survive until 15 is
# estimated to be 0.5
# 7 is the lower bound of the 95% CI for the median survival time
# The upper bound is infinity since the number of  is so small

summary(km.model)

# n.risk is the number of people at risk at the corresponding time
# n.event is the number of events at the corresponding time
# survival is the probability of surviving pass the corresponding time
# std.err is the sd of the estimated probability
# the 95% confidence interval is given

# The CI is very large because the number of sample is so small

plot(km.model, conf.int = F, xlab = "Time (months)", ylab = "S(t)",
     main = "Kaplan Meier Model", las = 1)

# The las argument turns the y axis label for readibility
# The CI can be plotted if wanted to

plot(km.model, conf.int = T, xlab = "Time (months)", ylab = "S(t)",
     main = "Kaplan Meier Model", las = 1, col = c("black", "red", "red"))

# Why the median is 15 and the upper bound of 95% CI is NA
abline(h = 0.5, col = "blue")

# The horizontal line cross the lower bound of the 95% CI but not the upper
# bound. That is why the upper bound is NA. It crossed the lower bound of 95%
# CI at time = 7, that is why the lower bound of the median is 7. It croseed
# the observed survival function at time = 15, that is why the median is 15.

# Visualizing the censor
plot(km.model, conf.int = T, xlab = "Time (months)", ylab = "S(t)",
     main = "Kaplan Meier Model", las = 1, col = c("black", "red", "red"),
     mark.time = T)

# The tick marks are when the censoring happened




# Survival models of Grouped data
# Creating the data.frame that the video used

AIDSsurv_group <- data.frame(
  Time = c(2,3,6,6,7,10,15,15,16,27,30,32,1,1,1,1,2,3,3,9,22),
  Event = c(1,0,1,1,1,0,1,1,1,1,1,1,1,0,1,1,1,1,1,1,0),
  Over40 = c(0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1))

# Creating the KM model for the new data
km.model.group <- survfit(Surv(AIDSsurv_group$Time, AIDSsurv_group$Event) ~
                            AIDSsurv_group$Over40)

# Evaluating the result
km.model.group

summary(km.model.group)

# The function simply created 2 different survival model for the 2 categories.
# The first model is exactly the same as the model we created before for the
# AIDSurv data since we are just adding 9 new observations representing people
# with age > 40

# Visualizing the estimated survival function
plot(km.model.group, conf.int = F, xlab = "Time (months)", ylab = "S(t)",
     main = "Kaplan Meier Model", las = 1, mark.time = T, col = c("red","blue"))

# With the CI
plot(km.model.group, conf.int = T, xlab = "Time (months)", ylab = "S(t)",
     main = "Kaplan Meier Model", las = 1, mark.time = T,
     col = c("black","red","blue"))


# Performing the Log-Rank Test
# The Null hypothesis is that the survival functions of the different groups
# are the same. The alternative hypothesis is that the survival functions are
# different

survdiff(Surv(AIDSsurv_group$Time, AIDSsurv_group$Event)~AIDSsurv_group$Over40)

# The Null hypothesis is rejected under the 5% significance level. The 2 groups
# have different survival functions 