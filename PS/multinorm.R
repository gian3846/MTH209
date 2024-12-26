library(mvtnorm)
library(pracma)

mu <- c(3, 5)
sig <- matrix(c(4, 2, 2, 3), 2, 2, T)

x <- rmvnorm(1e3, mu, sig)
plot(x[,1], x[,2], pch = 18)

y <- x - mu
plot(y[,1], y[,2], pch = 18)

rotation <- function(ang)
{
  x <- matrix(c(cos(ang), -sin(ang), sin(ang), cos(ang)), 2, 2, T)
  return(x)
}

y <-  x %*% rotation(pi/4)
plot(y[,1], y[,2], pch = 18)

y <-  x %*% rotation(pi/2)
plot(y[,1], y[,2], pch = 18)

eig <- eigen(sig)
eigVec <- eig$vectors
eigVal <- eig$values

trans <- eigVec %*% sqrt(diag(1/eigVal)) %*% t(eigVec)
y <- (x - mu) %*% trans
plot(y[,1], y[,2], pch = 18)

library(lpSolve)
A <- matrix(c(0.3, 0.6, 1, 1, 0, 1, 1, 0, 0, 1), 5, 2, T)
b <- c(18000, 50000, 20000, 0, 0)
dir <- c("<=", "<=", "<=", ">=", ">=")
obj <- c(.3, .5)
ans <- lp("max", obj, A, dir, b)  
