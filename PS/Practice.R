library(ISLR)
dat <- Smarket
par(mfrow = c(1,2))
plot(1:1250, dat[,3], type = 'l') # Line plot for previous 2 yrs
plot(1:242, dat[1:242,3], type = 'l')


library(matlib)
A <- matrix(c(1,-2,-1,2,3,2,3,-2,1), nrow = 3, ncol = 3)
A
b <- c(6, -1, 2)
b
Solve(A, b)
plotEqn3d(A,b, xlim=c(0,4), ylim=c(0,4))

diag(4)
A <- matrix(c(0, 1, 3, -1, -1, 1, -4, 0, 1, 0, 2, 4, 0, 1, 0, -4), 
            nrow = 4, ncol = 4, byrow = TRUE)
AI <- rowadd(A, 1, 2, -2)

df <- USArrests
df <- na.omit(df)
df <- scale(df)
d <- dist(df, method = "euclidean")

## Example 98
library(rgl)
library(ggplot2)
# Create some dummy data
dat <- replicate(2, 1:3)
# Initialize the scene, no data plotted
plot3d(dat, type = 'n', xlim = c(-1, 8), ylim = c(-1, 8), zlim = c(-10, 20), xlab = '', ylab = '', zlab = '')
# Define the linear plane
planes3d(2, 3, -1, 0, col = 'red', alpha = 0.6)
# Define the origin
points3d(x=0, y=0, z=0)

### Practical Applications

library(ISLR)
data(Auto)
dim(Auto)
auto <- Auto[,1:7]
new.data <- Auto[,1:8]
summary(auto)
pc <- prcomp(auto, scale=TRUE)
plot(pc)
plot(cumsum(pc$sdev^2/sum(pc$sdev^2)))
## Need this library
library(ggfortify)
autoplot(pc, data=new.data, colour = "origin")


#### 4_1
library(rgl)
dat <- replicate(2,1:3)
plot3d(dat, type = 'n', xlim = c(-8,8), ylim = c(-8,8), zlim = c(-8,8))
planes3d(1,1,1,0, col= 'purple',alpha = 0.6 )
points3d(x = 0,y = 0, z = 0)

plot3d(dat, type = 'n', xlim = c(-8,8), ylim = c(-8,8), zlim = c(-8,8))
planes3d(2,1,-1,0, col= 'purple',alpha = 0.6 )
points3d(x = 0,y = 0, z = 0)

plot3d(dat, type = 'n', xlim = c(-8,8), ylim = c(-8,8), zlim = c(-8,8))
planes3d(-3,2,5,0, col= 'purple',alpha = 0.6 )
points3d(x = 0,y = 0, z = 0)

plot3d(dat, type = 'n', xlim = c(-8,8), ylim = c(-8,8), zlim = c(-8,8))
planes3d(1,-1,-1,0, col= 'purple',alpha = 0.6 )
points3d(x = 0,y = 0, z = 0)

library(MASS)
A <- matrix(c(1, -1, 4, 2, 0, -1, -1, -1, 5), nrow=3, ncol=3, byrow=TRUE)
temp <- Null(t(A))
temp/temp[3]
library(pracma)

# 4_2
A <- matrix(c(1,-1,1,1,-2,-2,2,-1,2,1,3,2,-3,0,-2,1,4,-4,2,-1), nrow = 4, ncol = 5, byrow = TRUE )
nullspace(A)
Null(t(A))
rref(A)
orth(t(A))
t(rref(t(A)))
orth(A)
