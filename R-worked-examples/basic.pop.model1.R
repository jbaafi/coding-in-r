# Title: To solve stage-structured ODE model (simplified model)
# Date created: June 24, 2022
# Author: Joseph Baafi

# Clear workspace
rm(list = ls())

# set working directory
#setwd("/Users/jbaafi/Documents/coding-in-r/R-worked-examples")

# Load packages
pacman::p_load(pacman, deSolve, tidyverse)

# Define a function for the system of equations
pop.model <- function(t, y, ...){
  # The number of states
  E = y[1]
  L = y[2]
  P = y[3]
  A = y[4]
  # The system of equations (This model is based on Lutambi et al, 2013)
  dE <- b*phiA*A - (FE + muE)*E
  #dE <- b*(1 - A/k)*A - (FE + muE)*E
  dL <- FE*E - (FL + muL + deltaL*L)*L
  dP <- FL*L - (FP + muP)*P
  dA <- sigma*FP*P - (muA)*A
  return(list(c(dE, dL, dP, dA)))
}

# Model parameters (All parameter values are based on Lutambi et al, 2013)
b  = 200      # number of eggs laid per oviposition
phiA = 3.00   # oviposition rate
FE = 0.50     # egg hatching takes 2 days on average
muE = 0.56    #
FL = 0.14    # the larval period for the Anopheles mosquitoes is found to be 7 days
muL = 0.44
deltaL = 0.05
FP = 0.50    # the pupal period (1/FP) lasts for 2 days on average
muP = 0.37
sigma = 0.5  # Fraction of larvae that emerge as female mosquitoes
muA = 0.10 # 0.05 - 0.15 (This parameter is not clear with Lutambi so I can vary it)

param <- c(b, phiA, FE, muE, FL, muL, deltaL, FP, muP, sigma, muA)

# # Model parameters (All parameter values are based on Abdelrazec & Gumel, 2017)
# b  = 200     # number of eggs laid per oviposition
# k = 10^6   # oviposition rate 
# FE = 0.4     # egg hatching takes 2 days on average
# muE = 0.36   # 
# FL = 0.14    # the larval period for the Anopheles mosquitoes is found to be 7 days
# muL = 0.34
# deltaL = 0.01
# FP = 0.3    # the pupal period (1/FP) lasts for 2 days on average
# muP = 0.17
# sigma = 0.5  # Fraction of larvae that emerge as female mosquitoes
# muA = 0.05 # 0.05 - 0.15 (This parameter is not clear with Lutambi so I can vary it)
# 
# param <- c(b, FE, muE, FL, muL, deltaL, FP, muP, sigma, muA)
# Vector reproduction numbers
R0 <- b*sigma*FE*FL*FP/((muE+FE)*(muL+FL)*(muP+FP)*muA)
print(paste0("The value of R0 is ", R0))


# Initial conditions (I am not sure what values to use as initial conditions)
y.initial <- c(0.0, 0.0, 0.0, 10.0)
 
# Time steps (Period with which to run the simulation)
times <- seq(0, 200, by = 0.01)

# Numerical integration using the ode() function from deSolve package
out <-  ode(y = y.initial, func = pop.model, times = times, parms = param)

# Put the simulated values into a dataframe
out <- data.frame(out)

# # Define plot area as rows and columns to display each plot as a separate graph
# par(mfrow = c(4, 1), mar=c(1.8,4,1,2))    # 2 rows, 1 column
# 
# # Create plots
# plot(out$time, out$X1, type = "l", xlab = "Time (days)", ylab = "Egg density", 
#      pch = 19, col = "blue")     # plot no. 1
# 
# plot(out$time, out$X2, type = "l", xlab = "Time (days)", ylab = "Larval density", 
#      pch = 19, col = "green")     # plot no. 2
# 
# plot(out$time, out$X3, type = "l", xlab = "Time (days)", ylab = "Pupal density", 
#      pch = 19, col = "yellow")     # plot no. 3
# 
# plot(out$time, out$X4, type = "l", xlab = "Time (days)", ylab = "Adult females", 
#      pch = 19, col = "red")    # plot no. 2

par(mfrow = c(2, 2), mar=c(2,4,2,2))    # 2 rows, 2 columns

# Create plots
plot(out$time, out$X1, type = "l", xlab = "Time (days)", ylab = "Egg density", 
     pch = 19, col = "blue", main = "Popltn Dynamics(Constant Param)")     # plot no. 1

plot(out$time, out$X2, type = "l", xlab = "Time (days)", ylab = "Larval density", 
     pch = 19, col = "green")     # plot no. 

plot(out$time, out$X3, type = "l", xlab = "Time (days)", ylab = "Pupal density", 
     pch = 19, col = "yellow")     # plot no. 3

plot(out$time, out$X4, type = "l", xlab = "Time (days)", ylab = "Adult females", 
     pch = 19, col = "red")    # plot no. 2

# This dynamics is expected since the model does not consider environment carrying 
# capacity in any stage.


# Let us consider carrying capacity of eggs in the next file, basic.pop.model2.R


# x<-1:10
# par(mar=c(2.5,2.5,0.5,1))
# layout(matrix(c(1,2,3,4,1,5,3,6),ncol=2),heights=c(1,3,1,3))
# plot.new()
# text(0.5,0.5,"First title",cex=2,font=2)
# plot(x)
# plot.new()
# hist(x)
# boxplot(x)
# barplot(x)
