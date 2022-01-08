library(survival)
library(survminer)
data(lung)
View(lung)

# censoring status 
# 1: censoring time
# 2: failure time

# classify age into categories to treat it as a group indicator
lung$age <- ifelse(lung$age<65, '65-','65+')

# Research Interest 1
# KM survival estimates
fit.lung <- survfit(Surv(time, status)~age, data=lung)
# estimate of survival functions by age group.
summary(fit.lung) 
# KM survival curves for the two age groups
ggsurvplot(fit.lung, conf.int = T, pval = T, xlab = "Time in days", risk.table = T)


# Research Interest 2
# Logrank test
diff.fit.lung <- survdiff(Surv(time, status)~age, data=lung)
diff.fit.lung
