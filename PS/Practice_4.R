library(lpSolve)

A <- matrix(c(1, 0, 2, 3, 1, 1, 1, 0, 0, 1), 5, 2, byrow = TRUE)
b <- c(16, 19, 8, 0, 0)
obj <- c(5, 7)
dir <- c("<=", "<=", "<=", ">=", ">=")

sol <- lp("max", obj, A, dir, b)

#------------------------------------------------

A <- matrix(c(1, 1, 0, 4 ,3, -20, 1, 1, -10, 1, 0, -2), 4, 3, byrow = T)
b <- c(1000, 200, 500, 100)
dir <- c(">=", "<=", "<=", "<=")
obj <- c(30, 40, 80)
sol <- lp("min", obj, A, dir, b, all.int = T)
sol$solution
sol$objval

#------------------------------------------------

library(igraphdata)
library(pracma)

graph <- sample_gnp(20, 0.1)
plot(graph)
adj <- as_adjacency_matrix(graph)
lap <- graph.laplacian(graph, normalized = T)
nullspace(as.matrix(lap))
E <- eigen(as.matrix(lap))
sum(E$values)

data("macaque")
plot(as.undirected(macaque))
as_adjacency_matrix(macaque)
b <- as.matrix(graph.laplacian(as.undirected(macaque)))

#------------------------------------------------

carbon <- read.table("http://stat4ds.rwth-aachen.de/data/Carbon.dat", header=TRUE)
carbon
cut <- cut(carbon$CO2, breaks = 5)
table(cut(carbon$CO2, breaks = 5))

#------------------------------------------------

crime <- read.table("http://stat4ds.rwth-aachen.de/data/Murder2.dat", header = T)
boxplot(crime$murder~crime$nation)
tapply(crime$murder, crime$nation, sum)

#------------------------------------------------

guns <- read.table("http://stat4ds.rwth-aachen.de/data/Guns_Suicide.dat", header = T)
plot(guns$guns, guns$suicide)

#------------------------------------------------

PID <- read.table("http://stat4ds.rwth-aachen.de/data/PartyID.dat", header=TRUE)
table(PID$race, PID$id)
prop.table(table(PID$race, PID$id), margin = 2)
mosaicplot(table(PID$race, PID$id))

#------------------------------------------------

sheep <- read.table("https://stat4ds.rwth-aachen.de/data/Sheep.dat", header = T)
boxplot(sheep$weight~sheep$survival)
tapply(sheep$weight, sheep$survival, summary)

#------------------------------------------------

y <- rnorm(1e3, 5, sd = 6)
z <- (y - 5)/6
mean(z)
var(z)