#####################
# 1.1 WILCOX
#####################


# ONE SAMPLE WILCOXON SIGNED RANK TEST


# dataset containing the head circumference
set.seed(1234)
dataset = data.frame(name=paste0(rep("head_", 10), 1:10), circumference=round(rnorm(10,50,5), 1))
print(dataset)

values = dataset$circumference

print(wilcox.test(values, mu=25))
# p-value=0.001953 => p-value is less than the significance level alpha=0.05
# reject null hypothesis => average head circumference is different from 25

print(wilcox.test(values, mu=46))
# p-value=0.2324 => accept null hypothesis

print(wilcox.test(values))

custom_wilcox_data = rnorm(100, 0, 1)

custom_wilcox = function(data) {
    rank = sort(abs(data), decreasing = FALSE)

    t = 0
    for (item in data) {
        t = t + match(abs(item), rank) * (item > 0)
    }

    n = length(data)
    et = n * (n + 1) / 4
    dt = n * (n + 1) * (2 * n + 1) / 24

    pvalue = 2 * pnorm(-abs(t - et) / sqrt(dt))

    return (pvalue)
}

writeLines("\n><> ><> ><> ><> ><> ><> ><> ><> ><> ><>\n")

print(wilcox.test(custom_wilcox_data)$p.value)
print(custom_wilcox(custom_wilcox_data))

# PAIRED SAMPLES WILCOXON TEST

writeLines("\n><> ><> ><> ><> ><> ><> ><> ><> ><> ><>\n")

before = c(44, 39, 46, 51, 55, 60, 49)
after = c(54, 40, 42, 47, 50, 56, 47)

paired_dateset = data.frame(group=rep(c("before", "after"), each=7), circumference=c(before, after))
print(paired_dateset)

wilcox_paired_test = wilcox.test(before, after, paired=TRUE)
print(wilcox_paired_test)

# p-value = 0.3491 >= 0.05
# median head circumference before is not significally differnet from the median head circumference after
