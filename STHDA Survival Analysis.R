# Link to the web tutorial
# http://www.sthda.com/english/wiki/survival-analysis-basics

# Loading packages
library("survival")
library("survminer")

# Using the lung dataset from survival package
head(lung)

# Definitions of the variables

names(lung)


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

km.lung <- survfit(Surv(time, status)~sex, data = lung)
km.lung

# Since the predictor used is a categorical variable, the function creates 2
# Kaplan Meier Survival Model. One for each category. KM cannot utilize
# quantitative variables.

# We have more males than females. The females survive for longer time period
# than males. 

summary(km.lung)

km.lung.result <- surv_summary(km.lung)
attr(km.lung.result, "table")

# The surv_summary function creates a dataframe

km.lung$n.risk
km.lung$n.event
km.lung$n.censor
km.lung$strata

# n.risk is the number of people at risk just before time t
# n.event is the number of event precisely at time t
# n.censor is the number of people who exit the study precisely at time t

km.lung$surv

# surv is the probability to survive to time t

summary(km.lung)$table

# Visualizing the survival plot

plot(km.lung, col = c("red","blue"))

ggsurvplot(km.lung, pval = TRUE, conf.int= T,linetype = "strata",
           surv.median.line = "hv", ncensor.plot = T, conf.int.style = "step",
           legend.labs = c("Male", "Female"),
           ggtheme = theme_bw())

# THe plot clearly shows that males die faster than females.
# The CI gets wider the further right the plot because there are less and less
# sample overtime because of death and censoring.
# 

# pval = T shows the p-value 
# conf.int= T shows the confidence interval of the estimated survival function
# risk.table = T shows a table with the number of people at risk by time
# linetype = "strata" will use different line types for each strata
# ncensor.plot = T shows the number of censored subjects at each time t
# legend.labs change the label of the legend on top
# conf.int.style = "step" changes the style of the CI

# surv.median.line = "hv" point out where the median of each survival curve is
# the "hv" value means horizontal and vertical. Other values available are "h" 
# and "v"

# xlabel and ylabel can be changed using xlab and ylab

# Plotting the number of event (CDF =  1- SDF)

ggsurvplot(km.lung, ggtheme = theme_bw(), conf.int = T, fun = "event")

# Plotting the estimated cummulative hazard function

ggsurvplot(km.lung, ggtheme = theme_bw(), conf.int = T, fun = "cumhaz")

# Plotting the estimated SDF in percentage
ggsurvplot(km.lung, ggtheme = theme_bw(), conf.int = T, fun = "pct")

# fun can take many argument
ggsurvplot(km.lung, ggtheme = theme_bw(), conf.int = T,
           fun = function(y){-log(y)})

# -log(S(t)) is simply the CHF

# Performing log rank test
logrank.result <- survdiff(Surv(time, status) ~ sex, data = lung)
logrank.result

logrank.result$obs
logrank.result$exp
logrank.result$chisq

# The p-value is very small. We reject the null hypothesis that they have the
# same survival distribution.

# Creating a more complex KM model

km.lung <- survfit(Surv(time, status)~ sex + age, data = lung)
summary(km.lung)

# The function treats the quantitative variable age as a categorical variable.
# This is not what we want.

# Using the Colon dataset
head(colon)
?colon

# There are 3 types of rx and 2 types of adhere
# The help page does not specify what adhere = 0 and adhere = 1 mean
# rx is the type of treatment

table(colon$rx)
table(colon$adhere)

km.colon <- survfit(Surv(time, status) ~ sex + rx + adhere,data = colon)
summary(km.colon)
km.colon.result <- surv_summary(km.colon)

ggsurvplot(km.colon, conf.int = F, ggtheme=theme_bw())

# Too much cluter

ggsurvplot(km.colon, conf.int = F, ggtheme = theme_bw())$plot +
  facet_grid(rx~adhere) +
  theme(legend.position = "right")

# Sex = 0 means female and Sex = 1 means male








