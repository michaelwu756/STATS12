soil <- read.table("soil_complete.txt", header=TRUE)
linear_model <- lm(soil$lead ~ soil$zinc)
summary(linear_model)
plot(soil$lead ~ soil$zinc, xlab="Zinc Concentration (ppm)",
     ylab="Lead Concentration (ppm)", main="Lead vs. Zinc in Soil")
abline(linear_model, col="red", lwd=2)
plot(linear_model$residuals ~ soil$zinc, xlab="Zinc Concentration (ppm)",
     ylab="Lead Residuals (ppm)", main = "Lead vs. Zinc in Soil Residuals plot")
abline(a=0, b=0, col="red", lwd=2)

ice <- read.csv("sea_ice.csv", header = T)
ice$Date <- as.Date(ice$Date, "%m/%d/%Y")
linear_model2 <- lm(ice$Extent ~ ice$Date)
summary(linear_model2)
plot(ice$Extent ~ ice$Date, xlab="Date (Year)",
     ylab="Ice Extent (millions of km^2)", main="Ice Extent Over Time", type="l")
abline(linear_model2, col="red", lwd=2)
plot(linear_model2$residuals ~ ice$Date, xlab="Date (Year)",
     ylab="Ice Extent Residuals (millions of km^2)",
     main = "Ice Extent Over Time Residuals plot")
abline(a=0, b=0, col="red", lwd=2)