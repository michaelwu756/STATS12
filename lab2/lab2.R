library("maps")
library("mosaic")
flint <- read.csv("flint.csv")
mean(flint$Pb >= 15)
mean(flint$Cu[flint$Region == "North"])
mean(flint$Cu[flint$Pb >= 15])
mean(0 < flint$Pb & flint$Pb < 15)
mean(flint$Pb)
mean(flint$Cu)
boxplot(flint$Pb, main="Lead Levels in Flint Water", 
        xlab="Water Samples from Houses", ylab="Lead (Parts per Billion)")

life <- read.table("countries_life.txt", header=TRUE)
plot(life$Life~life$Income, col="blue", cex = 0.5, pch = 19,
     xlab = "Income per Capita ($)", ylab = "Life Expectancy (Years)",
     main = "Income vs Life Expectancy")
boxplot(life$Income, main="Income Boxplot", xlab="Income per Capita by Country", ylab="Amount ($)")
histogram(life$Income, main="Income Histogram", xlab="Income per Capita by Country ($)")
incomeBelow1000 <- life[life$Income < 1000, ]
incomeAtLeast1000 <- life[life$Income >= 1000, ]
plot(incomeBelow1000$Life~incomeBelow1000$Income, col="blue", cex = 0.5, pch = 19,
     xlab = "Income per Capita ($)", ylab = "Life Expectancy (Years)",
     main = "Income vs Life Expectancy for Income Below $1000")
cor(incomeBelow1000$Income, incomeBelow1000$Life)

maas <- read.table("soil.txt", header=TRUE)
summary(maas$lead)
summary(maas$zinc)
histogram(maas$lead, main="Lead in Maas River Bank Soil", xlab="Lead Parts per Million")
histogram(log(maas$lead), main="Lead in Maas River Bank Soil", xlab="Log(Lead Parts per Million)")
plot(log(maas$lead)~log(maas$zinc), col="blue", cex = 0.5, pch = 19,
     xlab = "Log(Zinc Parts per Million)", ylab = "Log(Lead Parts per Million)",
     main = "Lead vs Zinc in Maas River Bank Soil")
colors <- c("pink", "red", "darkred")
levels <- cut(maas$lead, c(0, 150, 400, 1000))
plot(maas$x,maas$y, xlab="x", ylab="y", main="Lead Concentration Map", "n")
points(maas$x,maas$y, cex=maas$lead/mean(maas$lead), col=colors[as.numeric(levels)], pch=19)

LA <- read.table("la_data.txt", header=TRUE)
plot(LA$Longitude,LA$Latitude, xlab="Longitude",
     ylab="Latitude", main="Los Angeles Neighborhood Centers", "n")
map("county", "California", add = TRUE)
points(LA$Longitude, LA$Latitude)
LAFiltered <- LA[LA$Schools > 0, ]
plot(LAFiltered$Schools~LAFiltered$Income, col="blue", cex = 0.5, pch = 19,
     xlab = "Income ($)", ylab = "School Performance",
     main = "School Performance vs Income")
mean(LA$Income)
sd(LA$Income)
mean(27152.29 < LA$Income & LA$Income < 89105.81)
mean(-3824.47 < LA$Income & LA$Income < 120082.57)
mean(-34801.23 < LA$Income & LA$Income < 151059.33)