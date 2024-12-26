x <- matrix(c(1, 2, 34, 4, 5, 67, 9 ,8, 7), 3, 3)
row.names(x) <- c("Jan", "Feb", "Mar")
colnames(x) <- c("Jan", "Feb", "Mar")

x[c("Jan":"Mar"),"Feb"]

x <- matrix(c(1, 2, 34, 4, 5, 67, 9 ,8, 7), 3, 3)
b <- c(2, 3, 4)

solve(x, b)

library(matlib)

Solve(x,b)
showEqn(x, b)
plotEqn3d(x, b)

echelon(x, b, verbose = TRUE)

##################################################

library(mvtnorm)
library(ggplot2)
library(matlib)

set.seed(123)
mu <- c(1, 2)
sigma <- matrix(c(4, 2, 2, 3), 2, 2)
samp <- rmvnorm(1e3, mean = mu, sigma = sigma)
E <- eigen(sigma)
sigma_root <- (E$vectors) %*% diag(c((E$values)^(-0.5))) %*% t(E$vectors)

rotated <- array(0, dim = dim(samp))
for(i in 1:1e3)
{
  rotated[i, ] <- sigma_root %*% (samp[i, ] - mu)
}

data <- as.data.frame(rotated)
p <- ggplot(data, aes(x = V1, y = V2)) + geom_point() + geom_density2d()
p

#################################################

library(pracma)

A <- matrix(c(1, 3, 2, 1, 2, 5, -2, -3, 2, 2, -3, -3, -4, -2, -1, 4), nrow = 4, ncol = 4, byrow=TRUE)
M <- cbind(A, diag(1, 4, 4))
R <- rref(M)
inv(A)

#################################################

A <- matrix(c(0, 1, 3, -1, -1, 1, -4, 0, 1, 0, 2, 4, 0, 1, 0, -4), 
            nrow = 4, ncol = 4, byrow = TRUE)

rowadd(A, 1, 2, 6)

#################################################

n <- 1e5
s <- 1:n
k <- 1/s
val <- numeric(length = n)
for(i in 1:n)
{
  t <- k[i]
  A <- matrix(c(t, t^2, t^3, t^2, t^4, t^6, t^3, t^6, t^9), 3, 3, byrow = TRUE)
  val[i] <- det(A)
}

plot(k, val)

#################################################

x <- c(-1, 1, 1, 3, 1+sqrt(2))
y <- c(1, -1, 3, 1, 1+sqrt(2))

M <- matrix(0, 5, 6)

for(i in 1:5)
{
  M[i, 1] <- x[i]*x[i]
  M[i, 2] <- x[i]*y[i]
  M[i, 3] <- y[i]*y[i]
  M[i, 4] <- x[i]
  M[i, 5] <- y[i]
  M[i, 6] <- 1
}

a <- det(M[ ,-1])
b <- -det(M[ ,-2])
c <- det(M[ ,-3])
d <- -det(M[ ,-4])
e <- det(M[ ,-5])
f <- -det(M[ ,-6])

#################################################

tri <- function(x1, y1, x2, y2, x3, y3)
{
  M <- matrix(c(x1, y1, 1, x2, y2, 1, x3, y3, 1), 3, 3, byrow = TRUE)
  ans <- 0.5*abs(det(M))
  return (ans)
}

area <- tri(0, 0, 2, 2, 1, -1) + tri(4, -1, 2, 2, 1, -1) + tri(5, 3, 2, 2, 4, -1) + tri(4, -1, 5, 3, 6, -2)