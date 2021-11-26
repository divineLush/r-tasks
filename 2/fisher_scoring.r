##########################################
# 2.1 FISHER'S SCORING ALGORITHM
##########################################

x = matrix(rexp(200, rate = .1), nrow = 10, ncol = 2)
y = matrix(c(rep(1, 10)), ncol = 1, byrow = FALSE)

fisher_scoring = function(y, x) {
    p = ncol(x)
    n = 20
    beta = matrix(rep(0, n*p), ncol = n)
    theta = matrix(rep(0, n*10), ncol = n)
    mu = matrix(rep(0, n*10), ncol = n)
    beta[,1] = runif(ncol(x))

    for (i in 1:(n - 1)) {
        # canonical link function
        theta[,i] = x %*% as.matrix(beta[,i])
        mu[,i] = exp(as.matrix(theta[,i])) / (1 + exp(as.matrix(theta[,i])))
        w = diag(as.vector(mu[,i] * (1 - mu[,i])))

        beta_i = as.matrix(beta[,i])
        inf = solve(t(x)%*% w %*% x) %*% t(x)
        score = y - as.matrix(mu[,i])

        beta[,i+1] = beta_i + inf %*% score
    }

    return (beta)
}

fisher = fisher_scoring(y,x)
print(fisher)
