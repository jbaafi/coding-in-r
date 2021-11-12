# Title: Example analysis for Ecuador
# Author: Joseph Baafi
# Date: 02-11-2021
# ------------------------

# Set working directory (commented out because works fine without)
#setwd("/Users/me/Desktop/cons-biol")

# Load data
data1 <- read.csv(file = "ecuador.csv")

# Remove the sides that are not marine and save the result as data2
data2 <- data1[data1$MARINE != 0,]

# Remove the NAs from the GIS_M_AREA column
data3 <- data2[is.na(data2$GIS_M_AREA)=="FALSE",]

# Calculate the mean and median sizes of the coastal MPAs
mean.area <- mean(data3$GIS_M_AREA)
median.area <- median(data3$GIS_M_AREA)

# Calculate the total area that is ‘no take’
area.no.take <- sum(data3$NO_TK_AREA)

# Count the number of MPAs in each ICU_CAT
data4 <- aggregate(x = data3$GIS_M_AREA,
                   by = list(data3$IUCN_CAT),
                   FUN = length)

# Remove row where values are 'Not Reported':
data4 <- data4[-3, ]

#Calculate the amount of area in each level of protected
data5<-aggregate(x = data3$GIS_M_AREA,
                 by = list(data3$IUCN_CAT),
                 FUN = sum)

# Remove row with no reported value
data5 <- data5[-3,]

# Making the plots
log10.area <- log(data3$GIS_M_AREA,10)
hist(log10.area, breaks = 50, xaxt="n", xlab = "x label here", ylab = "y label here", main = "title here")   # Histogram with logarithmic axis
axis(side = 1, at = seq(0, 6), labels = c(1, 10, 100, 1000, 10000, 100000, 1000000))

barplot(data4$x, xlab = "Level of protection", ylab="Number of areas",
        names.arg=data4$Group.1,
        las=1,
        main="Number of areas for each level of protection")

barplot(data5$x, xlab = "Level of protection", ylab="Area(Amt. land and water/km^2)",
        names.arg=data5$Group.1,
        las=3,
        main="Area of land and water for each level of protection")
