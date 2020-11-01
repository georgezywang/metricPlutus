y = Y[[2]]
X = matrix(nrow = length(y), ncol = 8)
for (i in 1:8){
  X[, i] = Y[[i+2]]
}
y = y[1:10000]
X = X[1:10000, ]

#do data normalization
for (i in 1:8){
  if (i != 3 && i != 7){
    X[, i] = X[, i]-mean(X[, i])
    X[, i] = X[, i]/sqrt(var(X[, i]))
  }
  else{
    X[, i] = X[, i]/X[1, i]
  }
}

#do dimensionality reduction
burn_in = 1000 #number of burn-in iterations
effective = 4000 #number of effective iterations
full_model = 500 #first 500 draws are drawn from the full model
S = burn_in+effective #number of samplings

res = list()
#the first column is delta, the indicator r.v.
#the second column is sigma, the error variance
#the third column in mu, the mean
#the fourth column is omega, the inclusion prob
#the fifth column is alpha, the regression para

N = length(y) #number of measurements
d = 8 #number of covariates
g = N #for g-slab
c = 1 #can choose different c, for i-slab
b = 1/N #for f-slab
r = 0.0001
mean.y = mean(y)
var.y = var(y)
sigma_0 = var.y #initialize error variance
mu_0 = mean.y #initialize mean
y_c = y-c(rep(1, N))*mean.y
delta_0 = c(rep(1, d)) #initialize delta as (1, 1, ..., 1)
phi_0 = c(rep(1, d)) #initialize phi as (1, 1, ..., 1)

set.seed(11-1-2020)
#initialization for omega
a_omega = 1; b_omega = 1 #uninformative prior
omega_0 = rbeta(1, a_omega, b_omega)

#two initializations for correlation matrix C
#independent regressors
#C = diag(40) 
#correlated regressors
rou = 0.8 #correlation
C = matrix(nrow = N, ncol = N) 
for (j in 1:N){
  for (k in 1:N){
    C[j, k] = rou^(abs(j-k))
  }
}

#initialization for NMIG prior
v = 5 ; Q = 4 #degree of freedom is 2*v

#initialization for alpha
alpha_0 = solve(t(X) %*% X) %*% t(X) %*% y

currSample = list(mu_0, delta_0, phi_0, omega_0, alpha_0, sigma_0)
res = list.append(res, currSample)
delta = rep(0, d)

for (s in 2:S){
  #sample mu
  mu_n = rnorm(1, mean.y, sqrt(currSample[[6]]/N))
  currSample[[1]] = mu_n
  
  #NMIG, student spikes and slabs
  delta_n = vector(mode = "numeric", length = d)
  phi_n = vector(mode = "numeric", length = d)
  delta_prev = currSample[[2]]
  omega_prev = currSample[[4]]
  alpha_prev = currSample[[5]]
  for (j in 1:d){
    p_slab = dt.scaled(alpha_prev[j], 2*v, 0, (2*v*Q/(2*v-2)*v))
    p_spike = dt.scaled(alpha_prev[j], 2*v, 0, r*(2*v*Q/(2*v-2)*v))
    L_j = p_spike/p_slab
    delta_n[j] = rbinom(1, 1, 1/(1+(1-omega_prev)*L_j/omega_prev))
    if (delta_n[j] == 1){
      phi_n[j] = rinvgamma(1, v+1/2, Q+alpha_prev[j]^2/2)
    }
    else{
      phi_n[j] = rinvgamma(1, v+1/2, Q+alpha_prev[j]^2/(2*r))
    }
  }
  currSample[[2]] = delta_n
  currSample[[3]] = phi_n
  delta = delta+delta_n
  
  #sample omega
  d_1 = 0
  for (j in 1:d){
    d_1 = d_1+delta_n[j]
  }
  omega_n = rbeta(1, a_omega+d_1, b_omega+d-d_1)
  currSample[[4]] = omega_n
  
  #sample alpha
  sigma_prev  = currSample[[6]]
  D = diag(d)
  for (j in 1:d){
    if (delta_n[j] == 1){
      D[j, j] = phi_n[j]
    }
    else{
      D[j, j] = phi_n[j]*r
    }
  }
  A_n = solve((1/sigma_prev)*(t(X)%*%X)+solve(D))
  a_n = A_n%*%t(X)%*%y_c/sigma_prev
  alpha_n = t(rmvnorm(1, a_n, A_n))
  currSample[[5]] = alpha_n
  
  #sample sigma
  s_n = (N-1)/2
  S_n = t(y_c-X%*%alpha_n)%*%(y_c-X%*%alpha_n)/2
  sigma_n = rinvgamma(1, s_n, S_n)
  currSample[[6]] = sigma_n
  
  res = list.append(res, currSample)
}

delta = delta/(S)
print(delta)