#CLEAR WORKSPACE
rm(list=ls())

#IMPORT DATA FILE 
# (Use 1 of the 3 commands below depending on the type of computer you are using)

# ***IMPORT ON A PC***
# df <-read.csv("C:/Downloads/lab7data.csv")
# OR
# df <-read.csv(file = "C:/Users/yrtze/Downloads/lab7data.csv")

# ***IMPORT ON A MAC*** 
 df <- read.csv("~/Downloads/lab7data.csv")

# ***IMPORT ON RCLOUD***
# df <- read.csv("/cloud/project/lab7data.csv")


# Work through ALL 4 Methods below to generate the graph for your report


#METHOD 1

# Plot 1: Elk abundance
plot(x=df$Year, y=df$Elk.Abundance, pch=20, col="blue", type ="b",
     xlab="Year",
     ylab="Elk Abundance")

# Plot 2: Wolves abundance
plot(x=df$Year, df$Wolf.abundance, type="b", col="red", pch=18,
     xlab="Year",
     ylab="Wolf Abundance")

# Plot 3: Percent aspen browsed
plot(x=df$Year, y=df$X..Aspen.browsed, pch=17, col="orange",
     type="b",
     xlab="Year",
     ylab="Percent of aspen browsed (%)")

# Plot 4: Mean height of aspen
plot(x=df$Year, y=df$Mean.aspen.height..cm., type="b", pch=16, col="darkred",
     xlab="Year",
     ylab="Mean height of aspens (cm)")

#METHOD 2

# Creating the first plot
plot(x=df$Year, y=df$Elk.Abundance, pch=20, col="blue", type ="b",
     xlab="Year",
     ylab="Abundance",
     ylim=c(0, 20000))

points(x=df$Year, df$Wolf.abundance, type="b", col="red", pch=18)

legend("topright", legend=c("Elk Abundance", "Wolf Abundance"), lty=1,col=c("blue", "red"), cex=1.5)

# Repeat the above steps to produce the second plot
plot(x=df$Year, y=df$X..Aspen.browsed, pch=17, col="orange",
     type="b",
     xlab="Year", ylab="Percent of aspen browsed/ height of aspen (cm)",
     ylim=c(0,250))

points(x=df$Year, y=df$Mean.aspen.height..cm., type="b", pch=16,
       col="darkred")

legend("topleft", legend=c("Percent of Aspen Broswed",
                           "Mean Aspen Height (cm)"), lty=1, col=c("orange", "darkred"), cex=1.5)

#METHOD 3

#Creating first plot
par(mar = c(5, 4, 4, 4) + 0.3)

plot(x=df$Year, y=df$Elk.Abundance, pch=20, col="blue", type ="b",
     xlab="Year",
     ylab="Elk Abundance")

par(new = TRUE)

plot(x=df$Year, y=df$Wolf.abundance, pch=18, col="red",
     type = "b",
     axes = FALSE,
     bty = "n",
     xlab = "", ylab = "")

mtext("Wolf Abundance", side=4, col="red", line=2)

axis(4, ylim=range(df$Wolf.abundance), col="red", col.axis="red",las=1)

#Creating second plot
par(mar = c(5, 4, 4, 4) + 0.3)

plot(x=df$Year, y=df$X..Aspen.browsed, pch=17, col="orange",
     type="b",
     xlab="Year", ylab="Percent of aspen browsed (%)",
     ylim=c(0,100))

par(new = TRUE)

plot(x=df$Year, y=df$Mean.aspen.height..cm., pch=15, col="darkgreen",
     type = "b",
     axes = FALSE,
     bty = "n",
     xlab = "", ylab = "")

mtext("Mean height of aspen (cm)",side=4,col="darkgreen",line=3)

axis(4, ylim=range(df$Mean.aspen.height..cm.), col="darkgreen",
     col.axis="darkgreen", las=1)

#METHOD 4

#Create first plot
par(mfrow=c(2,1))

par(mar = c(5, 4, 4, 4) + 0.3)

plot(x=df$Year, y=df$Elk.Abundance, pch=20, col="blue", type ="b",
     xlab="Year",
     ylab="Elk Abundance")

par(new = TRUE)

plot(x=df$Year, y=df$Wolf.abundance, pch=18, col="red",
     type = "b",
     axes = FALSE,
     bty = "n",
     xlab = "", ylab = "")

mtext("Wolf Abundance", side=4, col="red", line=2)

axis(4, ylim=range(df$Wolf.abundance), col="red", col.axis="red",las=1)

#Create second plot
par(mar = c(5, 4, 4, 4) + 0.3)

plot(x=df$Year, y=df$X..Aspen.browsed, pch=17, col="orange",
     type="b",
     xlab="Year", ylab="Percent of aspen browsed (%)",
     ylim=c(0,100))

par(new = TRUE)

plot(x=df$Year, y=df$Mean.aspen.height..cm., pch=15, col="darkgreen",
     type = "b",
     axes = FALSE,
     bty = "n",
     xlab = "", ylab = "")

mtext("Mean height of aspen (cm)",side=4,col="darkgreen",line=3)

axis(4, ylim=range(df$Mean.aspen.height..cm.), col="darkgreen",
     col.axis="darkgreen", las=1)

