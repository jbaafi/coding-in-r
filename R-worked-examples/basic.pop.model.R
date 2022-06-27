# Title: To solve stage-structured ODE model
# Date created: June 24, 2022
# Author: Joseph Baafi

# Clear workspace
rm(list = ls())

# set working directory
setwd("/Users/jbaafi/Documents/coding-in-r/R-worked-examples")

# Load packages
pacman::p_load(pacman, deSolve, tidyverse)

# Define a function for the system of equations
pop.model <- function(t, y, ...){
  # The number of states
  E = y[1]
  L = y[2]
  P = y[3]
  M = y[4]
  # The system of equations
  dE <- phi*b*(1-M/k)*M - (F_E + mu_E)*E
  dL <- F_E*E - (F_L + mu_L + delta_L*L)*L
  dP <- F_L*L - (F_P + mu_P)*P
  dM <- sigma*F_P*P - mu_M*M
  return(list(c(dE, dL, dP, dM)))
}

# Model parameters
b  = 10.7
phi = 2
k = 10^6
F_E = 0.4
mu_E = 0.36
F_L = 0.14
mu_L = 0.34
delta_L = 0.01
F_P = 0.3
mu_P = 0.17
sigma = 0.5
mu_M = 0.05

param <- c(b, phi, k, F_E, mu_E, F_L, mu_L, delta_L, F_P, mu_P, sigma, mu_M)

# Vector reproduction numbers
R0 <- phi*b*sigma*F_E*F_L*F_P/((mu_E+F_E)*(mu_L+F_L)*(mu_P+F_P)*mu_M)
print(paste0("The value of R0 is ", R0))


#Initial conditions
y.initial <- c(100, 0, 0, 100)
 
#time steps
times <- seq(0, 500, by = 0.01)

# Numerical integration. 
out <-  ode(y = y.initial, func = pop.model, times = times, parms = param)

out <- data.frame(out)

plot(out$time, out$X1, type = "l", xlab = "Time (days)", ylab = "Stage size", col = "yellow")
lines(out$X2, col = "red")
lines(out$X3, col = "blue")
lines(out$X4, col = "green")

