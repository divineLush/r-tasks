#####################
# 1.2 MANN-WHITNEY
#####################

# null hypothesis = no significant difference between apples and oranges
# alternative hypothesis = distribution of prices over apples and oranges is not the same

set.seed(5678)
apples = round(rnorm(8,50,5), 1)
oranges = round(rnorm(8,50,5), 1)

price = c(apples, oranges)
type = rep(c("apples", "oranges"), each=8)

dataset = data.frame(price, type, stringsAsFactors=TRUE)
print(dataset)

mann_whitney = wilcox.test(price~ type, data=dataset, exact=FALSE)
print(mann_whitney)

# p-value = 0.8748 > 0.5 => accept null hypothesis
# distribution of prices is the same => buy anynthing, prices won't matter


# HODGES-LEHMANN ESTIMATOR


vec = c()

for (i in 1:length(apples)) {
    vec[i] = oranges[i] - apples[i]
}

hl = median(vec)

print(vec)
print(hl)


# CONFIDENCE INTERVALS


print(wilcox.test(apples, conf.int=TRUE))
n = length(apples)
m = n * (n + 1) / 2
k = 1:(m / 2)
# psignrank - distribution function of the wilcoxon signed rank statistic
# confidence intervals have the form (W(k+1), W(m-k))
conf_lev = 1 - 2 * psignrank(k, n)
print(conf_lev)

# walsh averages
w = outer(apples, apples, "+") / 2
w = w[lower.tri(w, diag=TRUE)]
w = sort(w)

# k = 4, conf_lev = 0.9453
k = 4
n = length(apples)
m = n * (n + 1) / 2
interval = c(w[k + 1], w[m - k])
print("interval")
print(interval)

conf_lev = 1 - 2 * psignrank(k, n)
print("confidence level")
print(conf_lev)

