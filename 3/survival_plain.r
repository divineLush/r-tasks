# install.packages("survival")

library(survival)

# ?lung

Survival_Function = survfit(Surv(lung$time, lung$status == 2)~1, conf.type="plain")
print(Survival_Function$conf.int)

writeLines("\n><> ><> ><> ><> ><> ><> ><> ><> ><> ><>\n")

print(cbind(Survival_Function$lower, Survival_Function$upper))
