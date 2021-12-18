############################
# 3.1 CONFIDENCE INTERVALS
############################

# Kaplan-Meier estimator
# S(t) = Mult_{t_i <= t}(1 - (d_i / n_i))
# t_i -> observed death time
# n_i -> number of patients either alive and observed at t_i or else died at t_i
# d_i -> number of patients died at time t_i

# (1-a) * 100% confidence interval
# S(t) +- z_{a/2} * sqrt(Var(S(t)))
# where Var(S(t)) = S(t)^2 * Sum_{t_i <= t}(d_i / (n_i * (n_i * d_i)))
# and z_{a/2} is a-th quantile of the normal distribution

library(survival)

df = aml[order(aml$time), 1:2]
df_dead = df[df$status == 1, ]

ti = unique(df$time, 1)
di = c()
ni = c()

for (t in ti) {
    di = c(di, length(df_dead[(df_dead$time == t), 1]))
    ni = c(ni, length(df[df$time >= t, 1]))
}

# Kaplan-Meier estimator of the survival function
st = c()
for (i in 1:(length(ti))) {
    to_add = 1

    for (j in 1:length(ti)) {
        if (ti[j] <= ti[i]) {
            to_add = to_add * (1 - di[j] / ni[j])
            # print(c(di[j], ni[j]))
        }
    }

    st = c(st, to_add)
}

sum = c()
for (i in 1:length(ti)) {
    to_add = 0
    for (j in 1:length(ti)) {
        if (ti[j] <= ti[i]) {
            to_add = to_add + di[j] / ni[j] / (ni[j] - di[j])
        }
    }

    sum = c(sum, to_add)
}

d = c()
for (i in 1:length(st)) {
    d = c(d, ((st[i] ^ 2) * sum[i]))
}

z = qnorm(1 - .05 / 2)

# (1- alpha) * 100% Greenwood plain ci
ci_low = c()
ci_up = c()
d_sqrt = sqrt(d)
for (i in 1:length(st)) {
    ci_low = c(ci_low, st[i] - z * d_sqrt[i])
    ci_up = c(ci_up, st[i] + z * d_sqrt[i])
}

# log-log ci
dz = c()
ci_low_log = c()
ci_up_log = c()
for (i in 1:length(st)) {
    dz = c(dz, (1 / (log(st[i]) ^ 2)) * sum[i])

    log_st = log(-log(st[i]))
    dz_sqrt = sqrt(dz[i])

    c_minus = log_st - z * dz_sqrt
    c_plus = log_st + z * dz_sqrt

    ci_low_log = c(ci_low_log, exp(-exp(c_plus)))
    ci_up_log = c(ci_up_log, exp(-exp(c_minus)))
}

writeLines("\n><> ><> ><> ><> ><> ><> ><> ><> ><> ><> ><>\n")

print("plain:")
print(cbind(ci_low, ci_up))

writeLines("\n><> ><> ><> ><> ><> ><> ><> ><> ><> ><> ><>\n")

print("log-log:")
print(cbind(ci_low_log, ci_up_log))
