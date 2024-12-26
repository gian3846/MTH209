###### QUIZ I #####

#### When supply and demand is given and we have to find out the optimal plan to minimize the costs

A <- matrix(c(1 , 1 , 1 , 1 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ,
              0 , 0 , 0 , 0 , 1 , 1 , 1 , 1 , 0 , 0 , 0 , 0 ,
              0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 1 , 1 , 1 , 1 , 
              1 , 0 , 0 , 0 , 1 , 0 , 0 , 0 , 1 , 0 , 0 , 0 ,
              0 , 1 , 0 , 0 , 0 , 1 , 0 , 0 , 0 , 1 , 0 , 0 ,
              0 , 0 , 1 , 0 , 0 , 0 , 1 , 0 , 0 , 0 , 1 , 0 ,
              0 , 0 , 0 , 1 , 0 , 0 , 0 , 1 , 0 , 0 , 0 , 1),
            nrow = 7, ncol = 12, byrow = TRUE)

f.con <- rbind(A, diag(12))
f.rhs <- c(1000, 1500, 1200, 700, 900, 1200, 900, rep(0,12))

f.obj <- c(65,57,22,42,36,30,30,60,65,70,55,42)
f.dir <- c("=", "=", "=", "=", "=", "=", "=", ">=", ">=", ">=", ">=", ">=", ">=", ">=", ">=", ">=", ">=", ">=", ">=")

library(lpSolve)
Sol <- lp ("min", f.obj, f.con, f.dir, f.rhs)
Sol$objval
Sol$solution

greeks=c(alpha='\u03b1', beta='\u03b2', gamma='\u03b3')
data = matrix(Sol$solution, ncol = 4, byrow=TRUE)
colnames(data) = c('A', 'B', 'C', 'D')
rownames(data) = paste0(c(greeks['alpha'], greeks['beta'], greeks['gamma']))
final = as.table(data)
final


####### When we have to compute the adjacency matrix and the laplacian matrix 
library(pracma)


print(c("1-2", "1-3", "1-4", "2-3", "2-4", "2-6", "2-7", "3-4", "3-5", "4-6"))
print(c("1-3")) # share neighbor nodes 2, 4, and 6.

A = matrix(c(0, 1, 1, 1, 0, 1, 0, 0,          ### Adjacency matrix
             1, 0, 1, 1, 1, 0, 0, 0,
             1, 1, 0, 1, 0, 1, 1, 0,
             1, 1, 1, 0, 0, 0, 0, 0,
             0, 1, 0, 0, 0, 0, 1, 0,
             1, 0, 1, 0, 0, 0, 0, 1,
             0, 0, 1, 0, 1, 0, 0, 0,
             0, 0, 0, 0, 0, 1, 0, 0), nrow = 8, ncol = 8, byrow = T)

Deg = diag(c(4,4,5,3,2,3,2,1))             ### Degree matrix
L = Deg - A                               ### Laplacian matrix = Degree matrix - Adjacency matrix

LL <- L
LL[1,] <- LL[1,]/LL[1,1]
LL[2,] <- LL[2,]/LL[2,2]
LL[3,] <- LL[3,]/LL[3,3]
LL[4,] <- LL[4,]/LL[4,4]
LL[5,] <- LL[5,]/LL[5,5]
LL[6,] <- LL[6,]/LL[6,6]
LL[7,] <- LL[7,]/LL[7,7]
LL[8,] <- LL[8,]/LL[8,8]

E <- eigen(LL)
#E$values
EE = sum(E$values)

ifelse(EE == 8, TRUE, FALSE)     ### If sum of eigen values is equal to 8, then isolated vertices otherwise not

###### QUIZ II #######

set.seed(1)

### Problem 1###

n_seq <- c(1e1, 5e1, 1e2, 5e2, 1e3, 5e3)  ### sequence of sample size
N <- max(n_seq) 
alpha <- 2
beta <- 5

### draw a sample of maximum size from the Weibull distribution
y <- rweibull(N, shape = alpha, scale = beta)

##### log-likelihood function for Weibull
weib_like <-  function(a, y){
  n <- length(y)
  loglike <- n*log(alpha)+(alpha-1)* sum(log(y))-n*alpha*log(a)-sum((y/a)^(alpha))
  return(-loglike)
}

bet_mle_opt <- vector(length = length(n_seq))  ### beta mle using optim 
bet_mle_th <- vector(length = length(n_seq))   ### beta mle using theory
bet_seq <- seq(0.01, 20, length = 1e4) # a sequence of beta's
ll_eva <- matrix(nrow = length(bet_seq), ncol = length(n_seq)) ##log-likelihood eval for bet_seq  

#finding mle using optim and theory 
for (k in 1:length(n_seq)){
  s <- n_seq[k]
  bet_mle_opt[k] <- optim(beta, weib_like, y = y[1:s])$par
  bet_mle_th[k] <- (mean((y[1:s])^(alpha)))^(1/alpha)
  ll_eva[ ,k] <- sapply(bet_seq, weib_like, y[1:s])
}
bet_mle_th
bet_mle_opt

######Problem 2 ###
#### function to see if new observation lies in the confidence interval or not
est_prob <- function(theta, n, alpha){
  y <- rnorm(n, mean = theta, sd = 1)
  upper_ci <- mean(y) + (1/sqrt(n))*qnorm(1-alpha/2)
  lower_ci <- mean(y) - (1/sqrt(n))*qnorm(1-alpha/2)
  y101 <- rnorm(1, mean = theta, sd = 1)
  coverage <- ifelse(y101 > lower_ci & y101 < upper_ci, 1, 0)
  return(coverage)
}

iter_seq <- c(1e1, 1e2, 1e3, 1e4, 1e5) ### sequence of replications
iter_num <- max(iter_seq) ### number of maximum replications
store_cov <- vector(length = length(iter_num))

for (i in 1:iter_num) {
  store_cov[i] <- est_prob(10, 100, 0.05)
}

p_est <- vector(length = length(iter_seq))
p_sd <- vector(length = length(iter_seq))

for (i in 1:length(iter_seq)) {
  s <- iter_seq[i]
  p_est[i] <- mean(store_cov[1:s])
  p_sd[i] <- sd(store_cov[1:s])/sqrt(s)
}
p_est   # estimate of p for different replications
p_sd    # se of p for different replications
CI.up = p_est + p_sd
CI.dn = p_est - p_sd

###Plot of estimated p versus replication##
plot(p_est ~ log(iter_seq), xlab = "number of replictaions", ylab = "p_est", type = "l", ylim=c(0,0.3), col='red')
arrows(log(iter_seq), CI.dn, log(iter_seq), CI.up, code=3, length=0.2, angle=90, col='blue')
