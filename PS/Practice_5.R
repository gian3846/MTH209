y <- 1:12
plot(dbinom(y, 12, 0.5), type = "h")
mean <- sum(y*dbinom(y, 12, 0.5))
var <- sum((y^2)*dbinom(y,12,0.5)) - (sum(y*dbinom(y, 12, 0.5)))^2

GSS <- read.table("http://stat4ds.rwth-aachen.de/data/GSS2018.dat", header=T)
gender <- factor(GSS$SEX, levels = c(1, 2), labels = c("male", "female"))
race <- factor(GSS$RACE, levels = c(1, 2, 3), labels = c("Low", "Mid", "High"))
tab <- table(gender, race)
tab <- tab/sum(tab)
tab.margin <- addmargins(tab)

cond.prob.row <- prop.table(tab, 1)
cond.prob.col <- prop.table(tab, 2)

barplot((cond.prob.col))
abline(h = 0.5)
#------------------------------------------------

dmultinom(c(0, 1, 11), size = 12, prob = c(0.2, 0.3, 0.5))

#------------------------------------------------

height <- rnorm(1e3, 162, 7)
weight <- numeric(length = 1e3)
for(i in 1:1e3)
{
  weight[i] <- rnorm(1, 3+4*height[i], 8)
}
plot(x = height, y = weight)
mean(height); mean(weight)
sd(height); sd(weight)
corr <- sum((height - mean(height))*(weight - mean(weight)))/(sd(height)*sd(weight)*1e3)
corr <- cor(height, weight)

#------------------------------------------------

Afterlife <- read.table("https://stat4ds.rwth-aachen.de/data/Afterlife.dat", header = T)
belief <- factor(Afterlife$postlife, c(1, 2), c("Yes", "No"))
religion <- factor(Afterlife$religion, c(1, 2, 3), c("Protestant", "Catholic", "Jewish"))
tab <- table(belief, religion)
tab.prob <- tab/sum(tab)
tab.prob.mar <- addmargins(tab.prob)

barplot(tab.prob, legend.text = row.names(tab.prob))

cond.prob.row <- prop.table(tab.prob, 1)
cond.prob.col <- prop.table(tab.prob, 2)

barplot(cond.prob.col, legend.text = row.names(tab.prob))

#------------------------------------------------

X <- rbinom(1e3, 1, 0.5)
Y <- rbinom(1e3, 1, 0.5)
Z <- numeric(length = 1e3)
for(i in 1:1e3)
{
  if(X[i] == Y[i])
  {
    Z[i] <- 1
  }
  else
  {
    Z[i] <- 0
  }
}

first <- factor(X, levels = c(0, 1), labels = c("Tails", "Heads"))
second <- factor(Y, levels = c(0, 1), labels = c("Tails", "Heads"))
same <- factor(Z, levels = c(0, 1), labels = c("No", "Yes"))

tab <- table(first, second, same)
tab.prob <- tab/sum(tab)
tab.prob.mar <- addmargins(tab.prob)

#------------------------------------------------

qqnorm(rnorm(1e3, 0, 1))
abline(0, 1)

x <- rnorm(1e3, 0, 1)
u <- runif(1e3, -1, 1)
qqplot(x, u)

e <- rexp(1e3, 2.2)
qqplot(x, e)

#------------------------------------------------