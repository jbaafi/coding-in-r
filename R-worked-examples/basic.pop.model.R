# Title: To solve stage-structured ODE model
# Date created: June 24, 2022
# Author: Joseph Baafi

# Clear workspace
rm(list = ls())

# set working directory
#setwd("/Users/josephbaafi/Documents/GitHub/coding-in-r/R-worked-examples")

# Load packages
pacman::p_load(pacman, deSolve, tidyverse)

# Define a function for the system of equations
pop.model <- function(t, y, ...){
  # The number of states
  E = y[1]
  L = y[2]
  P = y[3]
  A = y[4]
  # The system of equations
  dE <- b*(1-A/k)*A - (F_E + mu_E)*E
  dL <- F_E*E - (F_L + mu_L + delta_L*L)*L
  dP <- F_L*L - (F_P + mu_P)*P
  dA <- sigma*F_P*P - mu_A*A
  return(list(c(dE, dL, dP, dA)))
}

# Model parameters
b  = 10.7
k = 10^6
F_E = 0.4
mu_E = 0.36
F_L = 0.14
mu_L = 0.34
delta_L = 0.01
F_P = 0.3
mu_P = 0.17
sigma = 0.5
mu_A = 0.05

parameters <- c(b, k, F_E, mu_E, F_L, mu_L, delta_L, F_P, mu_P, sigma, mu_A)

# Vector reproduction numbers
R0 <- b*sigma*F_E*F_L*F_P/(mu_E+F_E)*(mu_L+F_L)*(mu_P+F_P)

#Initial conditions
y0 <- c(100, 0, 0, 100)

#time steps
times <- seq(0, 100, by = 0.01)

# Numerical integration. 
out <-  ode(y = y0, func = pop.model, times = times, parms = parameters)
out <- data.frame(out)

plot(out$time, out$X1, type = "l", xlab = "Time (days)", ylab = "Stage size")
lines(out$X2, col = "red")

