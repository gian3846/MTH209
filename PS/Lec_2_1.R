library(matlib)
library(MASS)

A <- matrix(c(1, -2, -1, 2, 3, 2, 3, -2, 1), nrow = 3, ncol = 3)
inv(A)

B <- matrix(c(2, 5, 6, 2, 3, 1, 4, 2, -5), nrow = 3, ncol = 3)

AB <- A %*% B
BA <- B %*% A

AB == BA
all.equal(AB, BA)

A <- matrix(c(-5,-2,2,1,-1,5,5,3,3,-4,0,-3,-1,2,-1,-1,-5,-3,-2,-3,4,5,-2,-3,-4), 5, 5)
inv(A)
fractions(inv(A))
