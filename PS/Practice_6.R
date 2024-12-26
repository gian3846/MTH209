# dice.rolls <- function(iter)
# {
#   samp.mean <- numeric(length = iter)
#   y1 <- sample(1:6, iter, replace = T)
#   y2 <- sample(1:6, iter, replace = T)
#   samp.mean <- (y1 + y2)/2
#   hist(samp.mean, probability = T)
#   x <- (0:60)/10
#   lines(x, dnorm(x, 3.5, 1.5))
# }
# 
# dice.rolls(1e3)

# CLT_binom_sq <- function(iter, size, prob)
# {
#   Y <- rbinom(iter, size, prob)
#   Y_mean <- Y/size
#   Y_mean_sq <- Y_mean^2
#   hist(Y_mean_sq, probability = T)
#   
#   x <- (-10:50)/100
#   y <- dnorm(x, 0.25, sqrt((0.5*0.5)/size))
#   lines(x, y, col = "blue", lwd = 4)
# }
# 
# CLT_binom_sq(1e3, 100, 0.5)

# library(matlib)
# 
# A <- matrix(c(8, -9, 1, 10, -9, -7, -3, -7, 7, -25, -1, 13, -1, -16, -2, 3), 4, 4, byrow = T)
# b <- c(25, 16, 60, 41)
# Solve(A, b)
# echelon(A, reduced = T)
# 
# A <- matrix(c(0, -1, 1, 0, 1, 1, 0, 1, 3, -4, 2, 0, -1, 0, 4, -4), 4, 4)
# b <- c(1, 1, 5, -2)
# echelon(A,b)
# Solve(A,b)

# library(mvtnorm)
# library(ggplot2)
# 
# sigma <- matrix(c(4, 2, 2, 3), 2, 2)
# mu <- c(1, 2)
# x <- rmvnorm(n = 1e3, mean = mu, sigma = sigma)
# d <- data.frame(x)
# p2 <- ggplot(d, aes(x = X1, y = X2)) + geom_point(alpha = .5) + geom_density_2d()
# p2

# library(pracma)
# 
# v1 <- c(2, 3, 4)
# v2 <- c(3, 1, 6)
# cross(v1, v2)

# A <- matrix(c(1,-2,-1,2,3,2,3,-2,1), nrow = 3, ncol = 3)

# df <- as.data.frame(USArrests)
# dim(df)
# df.scaled <- as.data.frame(scale(df)) 
# 
# par(mfrow = c(1, 2))
# plot(df$Murder, df$Assault)
# plot(df.scaled$Murder, df.scaled$Assault)
# 
# df.pca <- prcomp(df, scale. = T, center = T)
# df.pca.x <- df.pca$x
# plot(df.pca$x[, 1], df.pca$x[, 2])
# ggplot(as.data.frame(df.pca.x), aes(x = PC1, y = PC2)) +
#   geom_point()

# A <- matrix(c(1, -1, 1, 1, -2, -2, 2, -1, 2, 1, 3, 2, -3, 0, -2, 1, 4, -4, 2, -1), 4, 5, byrow = T)
# nullspace(A)
# t(rref(t(A)))
# rref(A)

# v <- c(5, 6)
# B <- matrix(c(1, 1, 0, 1), 2, 2)
# inv(B) %*% v

# v <- c(0,3 ,3, 1, 0)
# B <- matrix(c(0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0,1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0), 5, 5)
# inv(B) %*% v

# A <- matrix(c(0, -2, -2, 0, 1, -3, 1, 1, -2, 0, 1, 3, 1, 2, 1, -1, 1, -2, 1, -2, 1, 1, 2, -3, 1, -3, 3, -3 ,-2, -3, -3, 0, -1, 1, -1, 0), 6, 6, byrow = T)
# e <- eigen(A)
# e$values
# inv(A)

# prob <- function(iter)
# {
#   samp <- vector(length = iter)
#   for(i in 1:iter)
#   {
#     u <- runif(1, 0, 1)
#     if(u <= 0.1)
#     {
#       samp[i] <- "00"
#     }
#     else if(u <= 0.4)
#     {
#       samp[i] <- "01"
#     }
#     else if(u <= 0.9)
#     {
#       samp[i] <- "10"
#     }
#     else
#     {
#       samp[i] <- "11"
#     }
#   }
#   return(samp)
# }
# 
# samp <- prob(1e3)
# table(samp)

# obj <- c(2, 7)
# A <- matrix(c(1, 1, 1, 7, -1, 1, 1, 0, 0, 1), 5, 2, byrow = T)
# b <- c(7, 35, 3, 0, 0)
# dir <- c("<=", "<=", "<=", ">=", ">=")
# library(lpSolve)
# sol <- lp("max", obj, A, dir, b)
# sol$objval

# library(igraph)
# gra <- sample_gnp(10, 0.2)
# plot(gra)
# as_adjacency_matrix(gra)
# laplacian_matrix(gra)
# laplacian_matrix(gra, normalized = T)
# library(pracma)
# nullspace(as.matrix(laplacian_matrix(gra)))