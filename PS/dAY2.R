library(matlib)
A <- matrix(c(1,9,8,7,6,5,4,3,2), nrow = 3, ncol = 3, byrow = TRUE)
b <- c(1,2,3)
solve(A, b)
Solve(A, b)
showEqn(A, b)
echelon(A, b, verbose = TRUE, fractions = TRUE)


# Exercise

A <- matrix(c(8,-9,1,10,-9,-7,-3,-7,7,-25,-1,13,-1,-16,-2,3), nrow = 4, ncol = 4, byrow = TRUE)
A
b <- c(25,16,60,41)
b
solve(A, b)
Solve(A, b)
echelon(A, b, verbose = TRUE, fractions = TRUE)
