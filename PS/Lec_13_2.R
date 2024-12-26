Happy <- read.table("http://stat4ds.rwth-aachen.de/data/Happy.dat", header=TRUE)

marital <- as.factor(Happy$marital)
happiness <- as.factor(Happy$happiness)
levels(marital) <- c("Marrried", "Separated", "NeverMind")
levels(happiness) <- c("Heaven", "Earth", "Hell")
tab <- table(happiness, marital)
marg <- addmargins(tab)
total <- sum(tab)

chi <- 0

for(i in 1:3)
{
  for(j in 1:3)
  {
    expt <- marg[i, 4] * marg[4, j] / total
    chi = chi + ((marg[i, j] - expt)^2)/expt
  }
}

chisq.test(happiness, marital)

#------------------------------------------------

LRT <- function(n, mu0, muHat)
{
  return( (2*n) * ((mu0 - muHat) - (muHat*log(mu0/muHat))) )
}
samp <- function(n, mu0R, mu0P, N)
{
  temp <- numeric(length = N)
  for(i in 1:N)
  {
    pois <- rpois(n, mu0R)
    muHat <- mean(pois)
    temp[i] <- LRT(n, mu0P, muHat)
  }
  return(temp)
}

stat <- samp(25, 5, 5, 1e4)
hist(stat, prob = T, breaks = "scott")

x <- seq(0, 20, length = 100)
y <- x
for(i in 1:length(x))
{
  y[i] <- dchisq(x[i], 1)
}
lines(x, y, lwd = 3, col = "blue")

#------------------------------------------------

n <- 400
jones <- 220
smith <- 219
prop.test(jones, n, 0.5, "two.sided")
prop.test(smith, n, 0.5, "two.sided")