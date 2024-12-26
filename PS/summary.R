load("AllData.Rdata")
dt <- screenTime
stratA <- dt[dt$Stratum == 'A', ]
stratB <- dt[dt$Stratum == 'B', ]

strataA <- stratA$Time.Spent
strataB <- stratB$Time.Spent

############################################3
set.seed (6)
x <- matrix ( rnorm (10 * 100), 10, 100)
x[, 1:50] <- x[, 1:50] + 0.5
t.test (x[, 1], mu = 0)
p.values <- rep (0, 100)
for (i in 1:100)
  p.values[i] <- t.test (x[, i], mu = 0)$p.value
decision <- rep ("Do not reject H0", 100)
decision[p.values <= .05] <- " Reject H0"
table (decision ,
       c( rep ("H0 is False ", 50), rep ("H0 is True ", 50))
)

x <- matrix ( rnorm (10 * 100), 10, 100)
x[, 1:50] <- x[, 1:50] + 1
for (i in 1:100)
  p.values[i] <- t.test (x[, i], mu = 0)$p.value
decision <- rep ("Do not reject H0", 100)
decision[p.values <= .05] <- " Reject H0"
table (decision ,
       c( rep ("H0 is False ", 50), rep ("H0 is True ", 50))
)

########################################################

pnorm(1)-pnorm(-1) # probability within 1 standard deviation of mean
pnorm(2)-pnorm(-2) # probability within 2 standard deviation of mean
pnorm(3)-pnorm(-3) # probability within 3 standard deviation of mean

# 0.05, 0.40, 0.95 quantiles of exponential

qexp(0.05,1) # distribution with parameter lambda= 1.0
qexp(0.40,1) # 0.40 quantile is-log(1 - 0.40)/1.0=0.511
qexp(0.95,1) # 0.95 quantile is -log(1 - 0.95)/1.0=2.996


X <- runif(1000000) # 1000000 randomly generated uniforms over [0,1]
Y <- -log(1- X)/(0.50)  # Here Y follows exponential distribution
mean(Y);sd(Y);