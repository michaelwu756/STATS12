library("maps")
library("mosaic")
heights <- c(71, 68, 72)
print(heights)
names <- c("Michael", "Hoang", "Huy")
print(names)
cbind(heights,names)
NCbirths <- read.csv("births.csv")
head(NCbirths)
find.package("maps")
map("state")
weights <- NCbirths$weight
weights.in.pounds = weights/16
weights.in.pounds[1:20]
mean(weights.in.pounds)
tally(NCbirths$Habit, "percent")
dotPlot(weights.in.pounds, width=1, main="Birth Weights", xlab="Weight (lbs)")
histogram(weights.in.pounds, nint=3, main="Birth Weights", xlab="Weight (lbs)")
histogram(weights.in.pounds, nint=20, main="Birth Weights", xlab="Weight (lbs)")
histogram(weights.in.pounds, nint=100, main="Birth Weights", xlab="Weight (lbs)")
boxplot(NCbirths$Fage, NCbirths$Mage, main="Parent's Ages", names=c("Father's Age", "Mother's Age"))
histogram(~weight | Habit, data = NCbirths, layout = c(1,2), width=10)
tally(~Habit | Premie, data = NCbirths, format = "proportion")
tally(~Habit | BirthDef, data = NCbirths, format = "proportion")
tally(~Habit | DelivComp, data = NCbirths, format = "proportion")
tally(~Habit | BirthComp, data = NCbirths, format = "proportion")
plot(weights.in.pounds ~ NCbirths$Mage, col="blue", cex = 0.5, pch = 19,
     xlab = "Mother's age", ylab = "Baby weight (lbs)",
     main = "Baby weight vs. Mother's age")
a <- read.table("http://www.stat.ucla.edu/~nchristo/statistics12/ozone.txt",
                header=TRUE)
AQI_colors <- c("lightblue", "blue", "darkblue", "purple4", "black")
AQI_levels <- cut(a$o3, c(0, 0.06, 0.075, 0.104, 0.115, 0.374))
plot(a$x,a$y, xlim=c(-125,-114),ylim=c(32,43), xlab="Longitude",
     ylab="Latitude", main="California ozone bubble plot", "n")
map("county", "ca",add=TRUE)
points(a$x,a$y, cex=a$o3/mean(a$o3),
       col=AQI_colors[as.numeric(AQI_levels)], pch=18)
