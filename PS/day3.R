### Practical Applications
A <- matrix(c(1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 
              0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ,
              0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ,
              1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 ,
              1 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ,
              1 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ,
              0 , 1 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ,
              0 , 1 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ,
              0 , 0 , 1 , 0 , 0 , 0 , 0 , 0 , 0 , 
              0 , 0 , 1 , 0 , 0 , 0 , 0 , 0 , 0 ,
              0 , 0 , 0 , 1 , 0 , 0 , 0 , 0 , 0 , 
              0 , 0 , 0 , 1 , 0 , 0 , 0 , 0 , 0 ,
              0 , 0 , 0 , 0 , 1 , 0 , 0 , 0 , 0 , 
              0 , 0 , 0 , 0 , 1 , 0 , 0 , 0 , 0 ,
              0 , 0 , 0 , 0 , 0 , 1 , 0 , 0 , 0 , 
              0 , 0 , 0 , 0 , 0 , 1 , 0 , 0 , 0 ,
              0 , 0 , 0 , 0 , 0 , 0 , 1 , 0 , 0 , 
              0 , 0 , 0 , 0 , 0 , 0 , 1 , 0 , 0 ,
              0 , 0 , 0 , 0 , 0 , 0 , 0 , 1 , 0 , 
              0 , 0 , 0 , 0 , 0 , 0 , 0 , 1 , 0 ,
              0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 1 , 
              0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 1), nrow = 11, ncol = 18, byrow = TRUE)
b <- c(1298, 1948, 465 , 605 , 451 ,  338 , 260 ,  183 ,  282 ,  127 ,  535)

############ library to be used

library(pracma)   

##########
E <- rref(A)
Augmented_mat <- cbind(A,b)
R1 <- rref(Augmented_mat)
R1 <- R1[-11,]

I <- eye(8)                     # identity matrix
O <- matrix(rep(0,80),8,10)     # zero matrix
b2 <- c(266, 223, 140, 264, 137, 67, 130, 24)
combine <- cbind(I,O,b2)
new <- rbind(R1,combine)
ans <- inv(new[,-19]) %*% new[,19]    # Solving eq from augmented matrix


diag(4)

B <- matrix(c(1, 3, 2, 1, 2, 5, -2, -3, 2, 2, -3, -3, -4, -2, -1, 4), nrow = 4, ncol = 4)
diagonal <- diag(4)
combine <- rref(cbind(B,diagonal))
inverse <- (combine[,5:8])

inv(B)

app2 <- cbind(new[,-19],diag(18))
reee <- rref(app2)
inver <-reee[,19:36]
ans <-inver %*% new[,19]
ans
