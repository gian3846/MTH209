library(mvtnorm)
library(matlib)
variance <- matrix(c(4,2,2,3), nrow = 2, ncol = 2) ### this should be positive semi-definite
x <- rmvnorm(n = 500, mean = c(0,0), sigma = variance)
pca <- princomp(x)
lod <- pca$loadings
d <- inv(lod) %*% t(x)
y <- t(d)
plot(y)
