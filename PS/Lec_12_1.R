data <- rnorm(100, 2, 5)

norm.ll <- function(theta, y)
{
  mu <- theta[1]
  sigma2 <- theta[2]
  n <- length(y)
  
  ll <- -n/2*log(2*pi) -n/2*log(sigma2) - (2*sigma2)^(-1)*sum((y-mu)^2)
  return(-ll)
}

ans <- optim(c(0, 2), norm.ll, y = data, method = "BFGS")

#------------------------------------------------

n <- 100
meanS <- numeric(length = 1e5)
medianS <- numeric(length = 1e5)
for(i in 1:1e5)
{
  samp <- rnorm(n)
  meanS[i] <- mean(samp)
  medianS[i] <- median(samp)
}
error.mean <- mean((meanS)^2)
error.median <- mean((medianS)^2)

#------------------------------------------------

n <- 10
meanS <- numeric(length = 1e5)
medianS <- numeric(length = 1e5)
for(i in 1:1e5)
{
  samp <- runif(n)
  meanS[i] <- mean(samp)
  medianS[i] <- median(samp)
}
error.mean <- mean((meanS)^2)
error.median <- mean((medianS)^2)

#------------------------------------------------

ll <- function(n, lambda, sumY)
{
  init <- n*log(lambda) - lambda*sumY
  return(init)
}

yBar <- 10
lam <- seq(0.01, 4, length = 100)
ll.diff <- numeric(length = length(lam))
# par(mfrow = c(2, 2))

N <- c(1, 5, 10)
# for(n in N)
# {
#   for(i in 1:length(lam))
#   {
#     ll.diff[i] <- ll(n, lam[i], yBar*n) - ll(n, 0.1, yBar*n)
#   }
#   plot(lam, ll.diff, lwd = 2, type = "l")
# }

ll.lam <- numeric(length = length(lam))
for(i in 1:length(lam))
{
  ll.lam[i] <- ll(10, lam[i], yBar*n)
}
plot(lam, ll.lam, lwd = 2, type = "n")
for(n in N)
{
  for(i in 1:length(lam))
  {
    ll.lam[i] <- ll(n, lam[i], yBar*n)
  }
  lines(lam, ll.lam, lwd = 2, type = "l", col = n)
}