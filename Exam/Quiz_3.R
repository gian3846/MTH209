#1
set.seed(1)
n <- 50
x <- numeric(length = n)
for(i in 1:n)
{
  u <- runif(1)
  if(u <= 0.333)
    x[i] <- rnorm(1, 0, 2)
  else
    x[i] <- rnorm(1, 0, 4)
}

y <- 7.5 + x*0.68 + rnorm(n, 0, 1)

fit <- lm(y~x)
#summary(fit)

coff <- fit$coefficients

plot(x, y, xlab = "covariate", ylab = "response", pch = 16)
l <- seq(-10, 10, length = 50)
m <- 7.5 + 0.68*l
lines(l, m, col = "blue")
m <- coff[1] + coff[2]*l
lines(l, m, col = "red")

n <- c(100, 200, 500, 1000, 5000)

ratioBeta0 <- numeric(length = length(n))
ratioBeta1 <- numeric(length = length(n))

for(i in 1:length(n))
{
  x <- numeric(length = n[i])
  for(j in 1:n[i])
  {
    u <- runif(1)
    if(u <= 0.333)
      x[j] <- rnorm(1, 0, 2)
    else
      x[j] <- rnorm(1, 0, 4)
  }
  
  y <- 7.5 + x*0.68 + rnorm(n, 0, 1)
  
  fit <- lm(y~x)
  coff <- fit$coefficients
  
  ratioBeta0[i] <- 7.5/coff[1]
  ratioBeta1[i] <- 0.68/coff[2]
}

par(mfrow = c(1, 2))
plot(log(n), ratioBeta0)
plot(log(n), ratioBeta1)

#2

dat <- read.csv("https://www.dropbox.com/s/geke5ykega8lytr/Q3_data.csv?dl=1")

y <- dat[ ,1] 
X <- dat[ ,-1]

fit <- glm(y~X1+X2+X3+X4, family = "binomial", data = X)
summary(fit)

pro <- predict(fit,type = "response")

yHat <- pro > 0.5
mean(y == yHat)