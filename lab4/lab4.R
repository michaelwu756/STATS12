library("mosaic")
pawnee <- read.csv ("pawnee.csv", header=TRUE)
head(pawnee)
dim(pawnee)
set.seed(1337)
pawneeSample <- pawnee[sample(nrow(pawnee), size=30),]
head(pawneeSample)
mean(pawneeSample$Arsenic)
mean(pawneeSample$New_hlth_issue=="Y")
mean(pawnee$New_hlth_issue=="Y")
histogram(pawnee$Arsenic, main="Arsenic Levels in Pawnee",
          xlab="Arsenic (ppm)", ylab="Homes", type="count")

n <- 30
N <- 541
M <- 1000
phats <- c()
set.seed(123)
for(i in 1:M){
  index <- sample(N, size = n)
  sample_i <- pawnee[index,]
  phats[i] <- mean(sample_i$New_hlth_issue == "Y")
}
histogram(phats, main="Sampling Distribution of Health Issue Proportions in Pawnee",
          xlab="Sample Health Issue Proportion", type="density", density=TRUE,
          fit="normal")
mean(phats)
sd(phats)

n <- 30
N <- 541
M <- 1000
phats <- c()
set.seed(123)
for(i in 1:M){
  index <- sample(N, size = n)
  sample_i <- pawnee[index,]
  phats[i] <- mean(sample_i$Arsenic)
}
histogram(phats, main="Sampling Distribution of Arsenic Levels in Pawnee",
          xlab="Sample Mean Arsenic (ppm)", type="density", density=TRUE,
          fit="normal")