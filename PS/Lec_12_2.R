library(binom)

response <- 1497
atheists <- 778
est <- atheists/response
lwr <- est - 1.96 * sqrt(est * (1-est) / response)
upr <- est + 1.96 * sqrt(est * (1-est) / response)

binom.confint(atheists, response)
binom.coverage(0.5, 1000, 0.95, "asymptotic")
binom.sim(M=1000, n=100, p=0.25, conf.level=0.95, methods="asymptotic")

#------------------------------------------------

Anor <- read.table("http://stat4ds.rwth-aachen.de/data/Anorexia.dat", header=TRUE)
change <- Anor$after - Anor$before
Anor<- cbind(Anor, change)

t.test(Anor$change[Anor$therapy == "cb"])
t.test(Anor$change[Anor$therapy == "cb"], Anor$change[Anor$therapy == "c"], var.equal = T, conf.level = 0.95)

cogbehav<-Anor$after[Anor$therapy == "cb"]-Anor$before[Anor$therapy == "cb"]
control<-Anor$after[Anor$therapy == "c"]-Anor$before[Anor$therapy == "c"]

t.test(cogbehav,control,var.equal=TRUE,conf.level=0.95)

#------------------------------------------------

Income <- read.table("http://stat4ds.rwth-aachen.de/data/Income.dat", header=TRUE)
boxplot(Income$income ~Income$race)
t.test(Income$income[Income$race == "B"], Income$income[Income$race == "W"], conf.level = 0.90, var.equal = T)
t.test(Income$income[Income$race == "B"], Income$income[Income$race == "W"], conf.level = 0.90, var.equal = F)

#------------------------------------------------

Substance <- read.table("http://stat4ds.rwth-aachen.de/data/Substance.dat", header=TRUE)
prop.test(c(sum(Substance$count[Substance$alcohol == "yes"]), sum(Substance$count[Substance$marijuana == "yes"])), c(sum(Substance$count), sum(Substance$count)))

#------------------------------------------------

samp <- rt(1e4, 3)
qqplot(samp, rnorm(1e4), type = "l")
abline(1, 1)
