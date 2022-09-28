# Working around plots in R

# Set working directory
setwd("/Users/jbaafi/Documents/coding-in-r")

# Sample plot
plot(1:10, ann = FALSE)

mtext(side = 1, text = "X Axis", line = 3)

mtext(side = 2, text = "Y Axis", line = 3)

# Another example with barplot
barplot(1:10, xlab = "x-axis")

# Another example with barplot
barplot(1:10, ann = FALSE)
mtext(side = 1, text = "X Axis", line = 3)
mtext(side = 2, text = "Y Axis", line = 3)

# Another plot
plot(1:10, (-4:5)^2, main = "Parabola Points", xlab = "xlab", ann = FALSE)
mtext(text = "x-axis", side = 1, line = 3)

# This one works on where the calibration of the axis should be
plot(1:10, mgp = c(3, 1, 0))

plot(1:10, mgp = c(3, 2, 0))
