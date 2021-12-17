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

ti_raw = c(lung$time)

ti = ti_raw[!duplicated(ti_raw)]
ti = sort(ti)

ni = c()
di = c()

ti_len = length(ti)

for (i in 1:(ti_len)) {
    # number of patients alive before times[i] or dead at times[i]
    n = 0
    # number of patients who died at times[i]
    d = 0

    cur = ti[i]
    for (raw in ti_raw) {
        if (cur <= raw) {
            n = n + 1
        }

        if (cur == raw) {
            d = d + 1
        }
    }

    ni[i] = n
    di[i] = d
}

# filter ti and return result length
filt_len = function(t) {
    return (length(ti[ti <= t]) )
}

# Kaplen-Meier estimator of the survival function
st = function(t) {
    fl = filt_len(t)

    mult = rep(1, fl) - di[1:fl] / ni[1:fl]
    mult = mult[mult != 0]

    return (prod(mult))
}

sum_helper = function(t) {
    fl = filt_len(t)

    di_slice = di[1:fl]
    ni_slice = ni[1:fl]

    d = di_slice / (ni_slice * (ni_slice - di_slice))
    d = d[d != Inf]

    return (sum(d))
}

d_st = function(t) {
    return ((st(t) ^ 2) * sum_helper(t))
}

# Greenwood's 95% confidence interval for st(t)
g_ci = function(t) {
    s = st(t)
    stderr = 1.96 * sqrt(d_st(t))

    return (c(s - stderr, s + stderr))
}

# log-log 95% confidence interval
log_ci = function(t) {
    s = st(t)
    sqrt_v = sqrt(sum_helper(t) / ((log(s)) ^ 2))

    log_s = log(-log(s))
    z = qnorm(.95)
    z_by_sqrt_v = sqrt_v * z

    c_plus = log_s + z_by_sqrt_v
    c_minus = log_s - z_by_sqrt_v

    return (c(exp(-exp(c_plus)), exp(-exp(c_minus))))
}

for (t in ti) {
    print(g_ci(t))
}

writeLines("\n><> ><> ><> ><> ><> ><> ><> ><> ><> ><>\n")

for (t in ti) {
    print(log_ci(t))
}
