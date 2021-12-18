# install.packages("survival")

library(survival)

Survival_Function_Plain = survfit(Surv(time, status)~1, data=aml, error="greenwood", type="kaplan-meier", conf.type="plain", conf.int=.95)

writeLines("\n><> ><> ><> ><> ><> ><> ><> ><> ><> ><>\n")

print("plain:")
print(Survival_Function_Plain$conf.int)
print(cbind(Survival_Function_Plain$lower, Survival_Function_Plain$upper))

Survival_Function_Log = survfit(Surv(time, status)~1, data=aml, error="greenwood", type="kaplan-meier", conf.type="log-log", conf.int=.95)

writeLines("\n><> ><> ><> ><> ><> ><> ><> ><> ><> ><>\n")

print("log-log:")
print(Survival_Function_Log$conf.int)
print(cbind(Survival_Function_Log$lower, Survival_Function_Log$upper))
