# Link to the web tutorial
# http://www.sthda.com/english/wiki/cox-proportional-hazards-model







# Loading package
library(survminer)
library(survival)

# We will use the lung data from the survival package

head(lung)
dim(lung)

# 228 observations and 10 variables

names(lung)

# Definitions of the variables
# inst: Institution code
# time: Survival time in days
# status: censoring status 1=censored, 2=dead
# age: Age in years
# sex: Male=1 Female=2
# ph.ecog: ECOG performance score (0=good 5=dead)
# ph.karno: Karnofsky performance score (bad=0-good=100) rated by physician
# pat.karno: Karnofsky performance score as rated by patient
# meal.cal: Calories consumed at meals
# wt.loss: Weight loss in last six months

# Creating CPH with age, sex, ph.ecog and wt.loss as covariates

cph.age.lung <- coxph(Surv(time, status)~age, data = lung)
summary(cph.age.lung)

# The z gives the Wald test statistics, exactly the same p-value as the one
# below

# age is significant since the p-value is < 0.05

cph.sex.lung <- coxph(Surv(time, status)~sex, data = lung)
summary(cph.sex.lung)

# The sex variables is encoded 1 for males and 2 for females.

# Sex is also significant. since the p-value is 0.001 < 0.05

# The coef for sex is -0.53. This indicates that being a female is associated
# with lower risk of death. We have built the KM model with sex as covariate
# before and we did get the result that females generally survive longer than
# males.

# the exp(coef) is the hazard ratio. the hazard ratio for females is 0.588.
# this indicates that being a female lowers the hazard by 58.8%.

cph.age.sex.lung <- coxph(Surv(time, status)~age+sex, data = lung)
summary(cph.age.sex.lung)

# Age is not significant anymore in the presence of sex

# Removing useless models

rm(cph.age.lung, cph.age.sex.lung)

# Finding the best model
cph.sex.phecog.lung <- coxph(Surv(time, status)~ph.ecog+sex, data = lung)
summary(cph.sex.phecog.lung)

# Testing other models
summary(coxph(Surv(time, status)~ph.ecog+ ph.karno + sex, data = lung))
summary(coxph(Surv(time, status)~ph.ecog+ meal.cal + sex, data = lung))
summary(coxph(Surv(time, status)~ph.ecog+ wt.loss + sex, data = lung))

# No variable manages to be significant in the presence of sex and ph.ecog
# We will use the CPH model with 2 covariate, that is, sex and ph.ecog

rm(cph.sex.lung)

# Visualizing the estimated baseline survival function
ggsurvplot(survfit(cph.sex.phecog.lung), data = lung, conf.int = T,
           ggtheme = theme_bw(), surv.median.line = "hv")
           

# Create the new data containing 2 people with ph.ecog = 1 and different sex
sex_df <- with(lung, data.frame(sex = c(1, 2),ph.ecog = c(1, 1)))
sex_df

# Visualizing the survival curve for the 2 new observations
ggsurvplot(survfit(cph.sex.phecog.lung, newdata = sex_df), data = lung)

# This is different than the KM model
km.lung <- survfit(Surv(time, status)~sex, data = lung)
km.lung

ggsurvplot(km.lung, conf.int= T,linetype = "strata",
           legend.labs = c("Male", "Female"),
           ggtheme = theme_bw())



