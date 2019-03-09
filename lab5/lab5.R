library("mosaic")
flint <- read.csv ("flint_2015.csv", header=TRUE)
p <- mean(flint$Pb >= 15)
pnorm(1.8487, lower.tail = FALSE)
prop.test(x = sum(flint$Pb >= 15), n = nrow(flint), p = 0.1, alt = "greater")
prop.test(x = sum(flint$Pb >= 15), n = nrow(flint), p = 0.1, alt = "greater",
          conf.level = 0.99)

p1 <- mean(flint$Pb[flint$Region == "North"] >= 15)
p2 <- mean(flint$Pb[flint$Region == "South"] >= 15)
n1 <- sum(flint$Region == "North")
n2 <- sum(flint$Region == "South")
2 * pnorm(3.57179, lower.tail = FALSE)
prop.test(x = c(p1 * n1, p2 * n2), n = c(n1, n2), alt = "two.sided")

mu <- mean(flint$Cu)
s <- sd(flint$Cu)
2 * pt(2.54415, df = nrow(flint) - 1, lower.tail = FALSE)
t.test(flint$Cu, mu = 40, alt = "two.sided")