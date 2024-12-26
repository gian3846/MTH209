Books <- read.table("http://stat4ds.rwth-aachen.de/data/Library.dat", header = TRUE)

N <- 1e5
meanBoot <- numeric(length = N)

for(i in 1:N)
{
  samp <- sample(Books$P, length(Books$P), replace = T)
  meanBoot[i] <- sd(samp)
}

hist(meanBoot)
ci <- c(mean(meanBoot) - 1.96*sd(meanBoot), mean(meanBoot) + 1.96*sd(meanBoot))

#------------------------------------------------

library(boot)

meanBoot <- boot(Books$P,function(x, b){median(x[b])}, R = 1e4)
ci <- boot.ci(meanBoot, conf = 0.95)
plot(meanBoot) 

corBoot <- boot(cbind(Books$C, Books$P), function(x,b){cor(x[b,1],x[b,2])}, R = 1e4) 
ci <- boot.ci(corBoot, conf = 0.90)

hist(corBoot$t, breaks = "Scott")

#------------------------------------------------

UN <- read.table("http://stat4ds.rwth-aachen.de/data/UN.dat", header=T)

yMean <- function(y, b)
{
  return(mean(y[b]))
}
yMedian <- function(y, b)
{
  return(median(y[b]))
}
yVar <- function(y, b)
{
  return(var(y[b]))
}
yCor <- function(y, b)
{
  return(cor(y[b, 1], y[b, 2], method = "pearson"))
}

bCor <- boot(cbind(UN$HDI, UN$Prison), yCor, R = 1e4)
plot(bCor)
boot.ci(bCor)
bCor$t0
plot(UN$HDI, UN$Prison)

#------------------------------------------------

yMean <- function(y, b)
{
  return(mean(y[b]))
}
gammaR <- function(y, mle)
{
  samp <- rgamma(length(y), shape = mle[1], rate = mle[2])
  return(samp)
}

y <- c(5.88,5.55,5.40,1.83,2.31,1.32,1.52,6.79,4.99,3.87,1.21,10.44,3.71,1.68,2.53,5.40,0.17,9.00,1.41,3.37,2.99,1.68,1.73,6.43,4.16)
s <- 2; r <- 0.5
mle <- c(s,r)

bootResults <- boot(y, yMean, R = 1e4, sim = "parametric", ran.gen = gammaR, mle = mle)
bootCI <- mean(y) + c(-1.96 * 0.567, 1.96 * 0.567)

#------------------------------------------------

samp <- rcauchy(1e2)

yMean <- function(y, b, trim)
{
  return(mean(y[b], trim))
}
yMedian <- function(y, b)
{
  return(median(y[b]))
}

bootResults <- boot(samp, yMean, trim = 0, R = 1e4)
bootCI <- boot.ci(bootResults)
bootCI

bootResults <- boot(samp, yMean, trim = 0.05, R = 1e4)
bootCI <- boot.ci(bootResults)
bootCI

bootResults <- boot(samp, yMedian, R = 1e4)
bootCI <- boot.ci(bootResults)
bootCI
