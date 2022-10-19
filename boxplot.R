# Boxplots

# Boxplots are a common type of graph that allow you to look at the relationships between a
# continuous variable and various categorical groups. They are super common in ecology because
# we often need to compare values between different categories.

# Clear workspace
rm(list = ls())

# Examples
# ----------------

# Boxplot of MPG by Car Cylinders. What this means is that the data for cyl (x-axis) is treated as
#      categorical while the mpg (y-axis) is continuous. You are able to compare means and median
#      (distribution of the data) for each category.
boxplot(mpg~cyl,data=mtcars, main="Car Milage Data",
        xlab="Number of Cylinders", ylab="Miles Per Gallon")


plot(mtcars$cyl, mtcars$mpg)

# Notched Boxplot of Tooth Growth Against 2 Crossed Factors
# boxes colored for ease of interpretation
boxplot(len~supp*dose, data=ToothGrowth,
        notch=TRUE,
        col=(c("gold","darkgreen")),
        main="Tooth Growth", xlab="Suppliment and Dose")

