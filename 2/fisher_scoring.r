##########################################
# 2.1 FISHER'S SCORING ALGORITHM
##########################################

data = read.csv('./House-Price-logit.csv')

x = as.matrix(cbind(1, data['price'], data['age']))
y = as.matrix(data['Sold'])

prob = function(x) {
    return (exp(x) / (1 + exp(x)))
}

fisher_matrix = function(x, beta) {
    p_x_beta = prob(x %*% beta)
    d = as.vector(p_x_beta * (1 - p_x_beta))

    return (solve(t(x) %*% diag(d) %*% x))
}

beta = matrix(rep(0, ncol(x)))
beta_p = 0

for (i in 1:10) {
    score = (t(x) %*% (y - prob(x %*% beta)))

    beta = beta + fisher_matrix(x, beta) %*% score
    beta_p = beta
}

sigma = sqrt(as.vector(diag(fisher_matrix(x, beta))))

print('sigma')
print(sigma)

writeLines("\n><> ><> ><> ><> ><> ><> ><> ><> ><> ><>\n")

print('beta')
print(as.vector(beta))

writeLines("\n><> ><> ><> ><> ><> ><> ><> ><> ><> ><>\n")

glm = glm(Sold ~ price + age, data = data, family = 'binomial')
print(glm$coefficients)
