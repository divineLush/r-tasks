#####################
# 1.1 WILCOX
#####################


# ONE SAMPLE WILCOXON SIGNED RANK TEST


# dataset containing the head circumference
set.seed(1234)
dataset = data.frame(name=paste0(rep("head_", 10), 1:10), circumference=round(rnorm(10,50,5), 1))
print(dataset)

wilcox_test = wilcox.test(dataset$circumference, mu=25)
print(wilcox_test)
# p-value=0.001953 => p-value is less than the significance level alpha=0.05
# reject null hypothesis => average head circumference is different from 25

wilcox_test = wilcox.test(dataset$circumference, mu=46)
print(wilcox_test)
# p-value=0.2324 => accept null hypothesis


# PAIRED SAMPLES WILCOXON TEST


before = c(44, 39, 46, 51, 55, 60, 49)
after = c(54, 40, 42, 47, 50, 56, 47)

paired_dateset = data.frame(group=rep(c("before", "after"), each=7), circumference=c(before, after))
print(paired_dateset)

wilcox_paired_test = wilcox.test(before, after, paired=TRUE)
print(wilcox_paired_test)

# p-value = 0.3491 >= 0.05
# median head circumference before is not significally differnet from the median head circumference after
