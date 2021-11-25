##########################################
# 2.1 FISHER'S SCORING ALGORITHM
##########################################

x = matrix(c(rep(1,10), log(c(1/128, 1/64, 1/32, 1/16, 1/8,1/4,1/2,1,2,4))), nrow = 10, ncol = 2) # x is on log scale.
y = matrix(c(0,0,0.4,0.4,0.6,0.8,1,1,1,1), ncol = 1, byrow = FALSE)   # y is proportion
m = 5 # m is equal for each x_i

fisher.scoring = function(y, x, start = runif(ncol(x))) {
  m = 5
  p = ncol(x)
  beta = matrix(rep(0,20*p),ncol = 20)
  score = rep(0, p)
  theta = matrix(rep(0,20*10), ncol =20)
  mu = matrix(rep(0,20*10), ncol = 20)
  beta[,1] = start

  for (i in 1:19) {
    # cannonical link function
    theta[,i] = x %*% as.matrix(beta[,i])
    mu[,i] = exp(as.matrix(theta[,i]))/(1+exp(as.matrix(theta[,i])))
    w = diag(as.vector(mu[,i] * (1-mu[,i])))
    beta[,i+1] = as.matrix(beta[,i]) + solve(t(x)%*% w %*% x) %*% t(x) %*% (y-as.matrix(mu[,i]))
  }
return (beta)
}

beta = fisher.scoring(y,x)

print(beta)
