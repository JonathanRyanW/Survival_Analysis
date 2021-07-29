# https://www.youtube.com/watch?v=TrS2M5imOt8f

# For the metadata please visit:
# https://www.openintro.org/data/index.php?data=heart_transplant

heart <- read.csv("./data/heart_transplant.csv")
head(heart)
sapply(heart, class)


# Heart data in the survival package
# https://stat.ethz.ch/R-manual/R-devel/library/survival/html/heart.html
data(heart, package = "survival")

# The jasa dataset is the original dataset
# jasa1 and heart is a processed data

# The dataset that was used in the video is the part of heart dataset that
# represents the people whom actually received the heart transplant. There are
# 157 of them in the video. We do not have any data with 157 observations.

names(heart)
head(heart)

# The age variable is the real age - 48 years
# year is the year of acceptance (calculated in years after 1 Nov 1967)
# surgery indicates whether prior bypass surgery is received. 1 means yes
# transplant indicates whether the observation received transplant. 1 means yes

table(heart$transplant)

# This is very weird. The metadata says that 1 indicates that the observation
# received the transplant. Therefore there should be 69 people in the dataset
# not 103. 103 is the number of people not receiving the transplant.

# It turns out that they are all receiving the transplant. There are only 103
# people in the study. Some people appears in multiple rows.

# It is very confusing but the dataset that most resembles the dataset used in
# the video is the stanford2 dataset.

names(stanford2)
?stanford2

# time is the survival time after receiving the transplant
# status is the censoring status, according to the video 1 means death, 0 means
# censored data
# age is in years
# t5 is the mismatch score


# Loading package
library(survival)

# Attaching heart for simpler typing
attach(stanford2)

# Building a regular CPH model
cph_stanford <- coxph(Surv(time, status) ~ age + t5)

# Evaluating the result
cph_stanford

# Oh it turns out that 27 observations are unuseable because the t5 values are
# NAs. That is why there are only 157 observations in the video. They already
# edit the data further.

# The coef for age is significant while the coef for t5 is not.
# The number of events is 102. As verified by this command

sum(na.omit(stanford2)$status)

summary(cph_stanford)

# The exp(coef) is the hazard ratio
# exp(-coef) is simply the reciprocal

# The exp(coef) for age is 1.03. This means that at a given point in time.
# If there are 2 person. One younger and one older. For each year additional
# year that the older person has compared to the younger, he/she is 3% more
# likely to die all else being the same.

# THe exp(coef)/hazard ratio is 1.186 which means that for each 1 additional t5
# the log hazard function is increased by 1.186

# The likelihood ratio test, wald test, and the logrank test are testing the
# same thing, that is, whether all of the coefficients are zero or not.
# The null hypothesis is that all the betas (coefficients of the covariates)
# are zero. Since every single p-value is lower than 0.05 then we will reject
# that null hypothesis in favor of the alternative hypothesis that at least
# one of them is not 0.

# Concordance is sometimes called the C-statistic. It measures the GOF of the
# survival model. It is the AUC in logistic regression.

# Suppose that there are 2 individuals. The model predicts that person A will
# live longer than person B. It turns out that in reality this happenned. Then
# the event is said to be in concordance.

# Concordance is the proportion of events in concordance to the model. The
# higher the concordance of a survival model, the better. 

# Should we drop the t5 variable?
# Building a model with and without the t5 variable
cph_age_t5 <- coxph(Surv(time, status) ~ age + t5)

stanford2_clean <- na.omit(stanford2)

detach(stanford2)
attach(stanford2_clean)

cph_age <- coxph(Surv(time, status) ~ age)

anova(cph_age, cph_age_t5, test = "LRT")

# The p-value is large. There is no statistical difference between the model.
# We should drop the t5 variable

rm(cph_age_t5, cph_stanford)

# Checking the linearity assumption
cph_age$residuals

plot(cph_age$residuals)
hist(cph_age$residuals)

# The residual plot looks random. But the histogram is not good. The model is
# consistently underestimating the probabilities.

# Partitioning the graphics device
par(mfrow = c(1,2))

# Plotting the Martingale Residuals

plot(predict(cph_age), residuals(cph_age, type = "martingale"),
     xlab = "fitted values", ylab = "Martingale Residuals", 
     main = "Martingale Residual Plot", las = 1)
abline(h=0, col = "blue")
lines(smooth.spline(predict(cph_age), residuals(cph_age, type = "martingale")),
      col = "red")

# The smoothed line is showing that the deviation are systematic. Showing
# non-linearity

# Plotting the Deviance Residuals
plot(predict(cph_age), residuals(cph_age, type = "deviance"),
     xlab = "fitted values", ylab = "Deviance  Residuals", 
     main = "Deviance Residual Plot", las = 1)
abline(h=0, col = "blue")
lines(smooth.spline(predict(cph_age), residuals(cph_age, type = "deviance")),
      col = "red")

# Same stuff. Systematic deviation from the line, signs of non-linearity

# Checking for Proportional Hazards
# The Schoenfeld test has the null hypothesis that the hazards are proportional
# the alternative hypothesis is that the hazards are not proportional

cox.zph(cph_age)

# All the p-values are larger than 0.05. We don't have any reason to assume
# that the hazard rate is not proportional. This CPH assumption is fulfilled

par(mfrow = c(1,1))
plot(cox.zph(cph_age))

# If we have more variables, there will be multiple plots showed for each of 
# the variable. Here the plot shows how the hazard ratio will change is we
# allow the age variable to change overtime. 0 change is what we want.

abline(h=0, col = "red")

# We want the red line to stay within the CI most of the time.
# The plot is giving signs that the change is mostly above zero, the hazard
# ratio will increase









