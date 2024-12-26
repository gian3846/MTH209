library(matlib)

A <- matrix(c(8, -9, 7, -1, -9, -7, -25, -16, 1, -3, -1, -2, 10, -7, 13, 3), nrow = 4, ncol = 4)
b <- c(25, 16, 60, 41)

solve(A,b)
Solve(A,b)

showEqn(A, b)

echelon(A, b, FALSE, verbose = TRUE)
