library(rgl)

dat <- replicate(2, 1:3)
plot3d(dat, type = "n", xlim = c(-5, 5), ylim = c(-5, 5), zlim = c(-5, 5))
planes3d(2, 3, -1, col = "red")
points3d(0, 0, 0)

#################################################

data <- iris
data.pca <- prcomp(iris[ ,1:4], scale. = TRUE)

library(ggfortify)
autoplot(data.pca, data = data, colour = "Species")

#################################################

library(rgl)

p <- matrix(0, 3, 2)
plot3d(p, type = "n", xlim = c(-5, 5), ylim = c(-5, 5), zlim = c(-5, 5))
planes3d(2, 2, 3, col = "lightblue")

#################################################

library(pracma)

A <- matrix(c(1, -1, 4, 1, 2, 0, -1, -2, -1, -1, 5, 3),nrow=3, ncol=4, byrow=TRUE)
rref(A)
t(rref(t(A)))
nullspace(A)

A <- matrix(c(1, -1, 1, 1, -2, -2, 2, -1, 2, 1, 3, 2, -3, 0, -2, 1, 4, -4, 2, -1), 4, 5, byrow = TRUE)
null <- nullspace(A)
col <- t(rref(t(A)))
row <- rref(A)

#################################################

library(matlib)

A <- matrix(c(-5, 5, -1, -7, -2, -4, 1, 3, 4), 3, 3, byrow = TRUE)
b <- c(57, 21, 3)

null <- nullspace(A)
col <- t(rref(t(A)))
row <- rref(A)
rank <- Rank(A)
Solve(A, b)

#################################################

B <- matrix(c(1, 0, 0, 1, 1, 0, 1, 1, 1), 3, 3, byrow = TRUE)
v <- c(2, 4, 0)
inv(B) %*% v

#################################################

u <- c(1, 1, 1, 1)
v <- c(1, 0, 1, 1)

proj <- sum(u*v)/sqrt(sum(u^2))

A <- matrix(c(3, 0, -2, -1, 0, 0, 1, 2, -3, -2, -2, 2, 2, -2, -1, -3, 3, -3, -3, -1, -1, -2, -3, 0), 4, 6, byrow = TRUE)
orth.row <- orth(t(A))
orth.col <- orth(A)
orth.null <- orth(nullspace(A))
