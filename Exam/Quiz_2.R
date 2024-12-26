set.seed(1)

#1
beta <- 5
n <- c(10, 50, 100, 500, 1000, 5000)
y <- list()

mleBeta <- function(y, n)
{
  sqrt(sum(y^2)/n)
}

negLogLike <- function(y, n, beta)
{
  logLikeli <- n*log(2) - 2*n*log(beta) + sum(log(y)) - sum(y^2)/(beta^2)
  return(-logLikeli)
}

mle_cal <- numeric(length = length(n))
mle_opt <- numeric(length = length(n))

for(i in 1:length(n))
{
  y[[i]] <- rweibull(n[i], shape = 2, scale = beta)
  mle_cal[i] <- mleBeta(y[[i]], n[i])
  mle_opt[i] <- optim(beta, negLogLike, y = y[[i]], n = n[i], method = "BFGS")$par
}

print(mle_cal)
print(mle_opt)

#2
set.seed(1)

mu0 <- 10
N <- 1e2
y <- rnorm(N, mu0, 1)

test <- t.test(y, mu = mu0)
conf <- test$conf.int

n <- c(10, 100, 1000, 10000, 100000)
p <- numeric(length = length(n))
sdP <- numeric(length = length(n))

for(i in 1:length(n))
{
  score <- numeric(length = n[i])
  for(j in 1:n[i])
  {
    samp <- rnorm(1, mu0, 1)
    
    if((samp <= conf[2]) && (samp >= conf[1]))
      score[j] <- score[j] + 1
  }
  p[i] <- mean(score)
  sdP[i] <- sd(score)
}

print(p)
print(sdP)
