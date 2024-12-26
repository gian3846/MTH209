solve(B)

library(mvtnorm)
library(ggplot2)
library(matlib)

## Standard deviation
sigma <- matrix(c(4,2,2,3), ncol = 2, nrow = 2) 

mat <- matrix(c(0, -1, 1, 0), 2, 2, byrow = TRUE)

## Mean
mu <- c(1, 2)
n <- 1000
set.seed(123)
x <- rmvnorm(n = n, mean = mu, sigma = sigma)
y <- array(0, dim = dim(x))
for(i in 1:dim(x)[1])
{
  y[i, ] <- mat %*% x[i, ]
}
d <- data.frame(y)
p2 <- ggplot(d, aes(x = X1, y = X2)) + geom_point(alpha = .5) + geom_density_2d()
p2


