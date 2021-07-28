# Link to the video tutorial in Youtube
# https://www.youtube.com/watch?v=zH7QkUacR04


# Loading the survival package
library(survival)

# Typing in the dataset
leukimia <- data.frame(
  Time = c(1,3,3,6,7,7,10,12,14,15,18,19,22,26,28,29,34,40,48,49,1,1,2,2,3,4,5,
           8,8,9,11,12,14,16,18,21,27,31,38,44),
  Event = c(rep(1,14),0,1,1,1,0,0, rep(1,16),0,1,0,1),
  Treatment = c(rep("A",20), rep("B",20)))

# There are only 2 types of treatment, A and B.
# Event 1 means remission. Event 0 means that the observation ends before
# remission is observed.

head(leukimia)
tail(leukimia)

# Attching the leukimia dataframe
attach(leukimia)

# Splitting the leukimia dataframe based on the treatment

leukimia_A <- Surv(Time[1:20], Event[1:20])
leukimia_B <- Surv(Time[21:40], Event[21:40])

leukimia_A
leukimia_B

class(leukimia_A)

# They are of a particular object called Surv

# Creating a KM model for the whole population
km.leukimia <- survfit(Surv(Time, Event) ~ 1, ctype = 1, stype = 1)

# Evaluating the model
km.leukimia
summary(km.leukimia)

# Plotting the estimated survival function
plot(km.leukimia, conf.int = F, xlab = "Time", ylab = "S(t)", las = 1,
     main = "Kaplan Meier Model",  mark.time = T)

# With 95% CI
plot(km.leukimia, conf.int = T, xlab = "Time", ylab = "S(t)", las = 1,
     main = "Kaplan Meier Model",  mark.time = T, col = c("black","red","red")) 

# Visualizing the median and it's 95% CI
abline(h = 0.5, col = "blue")

# Building KM model with grouping
km.leukimia.group <- survfit(Surv(Time, Event) ~ Treatment, ctype = 1, stype =1)

# Evaluating the results
km.leukimia.group

# The Treatment B has considerably lower median of remission time than Treatment
# A. Therefore Treatment B looks better since patients are recovering quicker.

summary(km.leukimia.group)

# Plotting the estimated survival function
plot(km.leukimia.group, conf.int = F, xlab = "Time", ylab = "S(t)", las = 1,
     main = "Kaplan Meier Model",  mark.time = T, col = c("red", "blue"))

# Adding legend
legend(33, 1.04, legend = c("Treatment A", "Treatment B"), lty = 1, lwd = 2,
       col = c("red", "blue"), bty = "", cex = 0.8)

# As expected, the survival function for the Treatment A is generally above the
# corresponding survival function for Treatment B. This is expected from looking
# at the median and their 95% CI.

# Visualizing the median and it's 95% CI
abline(h = 0.5, col = "green")


# Performing the Log-Rank test to the 2 Treatments
survdiff(Surv(Time, Event) ~ Treatment)

# We reject the null hypothesis that the 2 survival functions are the same in
# favor of the alternative hypothesis that they are different.

detach(leukimia)

# Building the NA model
NA_leukimia_A <- coxph(leukimia_A~1)
NA_leukimia_B <- coxph(leukimia_B~1)

NA_leukimia_A
NA_leukimia_B

# Extracting the estimated hazard function
basehaz(NA_leukimia_A)
basehaz(NA_leukimia_B)

# The force of mortality does not change when only censoring occur. It changes
# when the actual event occurs.

# Extracting residuals
NA_leukimia_A$residuals

# We can also extract other model components
NA_leukimia_A$nevent
NA_leukimia_A$method
NA_leukimia_A$n
NA_leukimia_A$concordance

# No summary
summary(NA_leukimia_A)

# Calculating the SE of the estimated hazard function
# First we need to find the number of events and number of people at risk at
# each time point for each treatment

dj_A <- km.leukimia.group$n.event[1:20]
dj_B <- km.leukimia.group$n.event[21:40]

nj_A <- km.leukimia.group$n.risk[1:20]
nj_B <- km.leukimia.group$n.risk[21:40]

time_a <- unique(leukimia$Time[1:20])
time_b <- unique(leukimia$Time[21:40])

variance_haz_A <- c()
variance_haz_A[1] <- dj_A[1]/nj_A[1]**2

for(i in 2:length(time_a)){
  variance_haz_A[i] <- variance_haz_A[i-1] + dj_A[i]/nj_A[i]**2
}

variance_haz_B <- c()
variance_haz_B[1] <- dj_B[1]/nj_B[1]**2

for(i in 2:length(time_b)){
  variance_haz_B[i] <- variance_haz_B[i-1] + dj_B[i]/nj_B[i]**2
}

# The standard deviations
sd_haz_A <- variance_haz_A**0.5
sd_haz_B <- variance_haz_B**0.5

# Building the NA model for the whole population
NA_leukimia <- coxph(Surv(leukimia$Time, leukimia$Event) ~ 1)

# Extracting the hazard function
basehaz(NA_leukimia)

# Comparing NA and KM
km.leukimia$std.chaz
km.leukimia$cumhaz

# Plotting the estimated hazard functions
plot(x = basehaz(NA_leukimia)$time, y = basehaz(NA_leukimia)$hazard, type = "s",
     xlab = "Time", ylab = "Hazard", main = "Nelson-Aalen Model")
lines(x = basehaz(NA_leukimia_A)$time, y = basehaz(NA_leukimia_A)$hazard,
      type = "s", col = "red")
lines(x = basehaz(NA_leukimia_B)$time, y = basehaz(NA_leukimia_B)$hazard,
      type = "s", col = "blue")
legend(0, 2.4, legend = c("Treatment A", "Treatment B"), lty = 1, lwd = 2,
       col = c("red", "blue"), bty = "", cex = 0.8)

# We can see that the hazard function of the Treatment B is in general higher
# than that of the Treatment A. That means people who received Treatment B is
# getting remissions quicker, they are recovering faster.














               