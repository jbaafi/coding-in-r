# Title: GILLESPIE SSA WITH TIME-DEPENDENT PARAMETERS 
# Date: 01/12/2021

# Clear workspace
rm(list = ls())

# Working directory
#setwd("/Users/jbaafi/Documents/coding-in-r/R-worked-examples")

# Load packages
library(GillespieSSA)
library(ssar)

# GILLESPIE SSA WITH TIME-DEPENDENT PARAMETERS url(https://githubmemory.com/repo/RodrigoZepeda/ssar)
# -----------------------
#EXAMPLE 1 
#------------------------
#Logistic Growth

# First, we set the seed for the simulation.
set.seed(123)

# Initial data must be inputed as a matrix.
X <- matrix(c(N=500), nrow = 1)

#The propensity vector should also be in matrix form:
v <- matrix( c(+1, -1), ncol = 2)

# The propensity scores must also be a matrix-valued function depdendent on 3 parameters: 
# time (t), the state of the system (X) and additional parameters (params) which we 
# discuss later.
pfun <- function(t,X,params){ cbind(2 * X[, 1], (1 + 1*X[, 1]/1000)*X[, 1]) }

#The model runs automatically from 0 to 1 conducting 10 simulations and generating a plot.
simulation <- ssa(X, pfun, v)

# The time for the simulation and number of simulations can be specified:
tmin       <- 0
tmax       <- 10
nsim       <- 20

# Plot characteristics can be specified by title, xlab and ylab:
simulation <- ssa(X, pfun, v, tmin, tmax, nsim, 
                  title = "Logistic Growth: Example 1", 
                  xlab = "Time", ylab = "Individuals")

# Making plots can really slow down the process. The option: plot.sim when set to FALSE 
# allows us to keep the data without making any plot:
simulation <- ssa(X, pfun, v, tmin = 2, tmax = 10, nsim = 20, plot.sim = FALSE)

#EXAMPLE 2
#------------------------
# 2. Time-dependent Logistic Growth

# Suppose we are using almost the same model as in the previous example:
set.seed(322)
X <- matrix(c(N=500), nrow = 1)   #Initial values
v <- matrix( c(+1, -1), ncol = 2) #Propensity scores

# But the propensity function now depends on time:
pfun <- function(t,X,params){ cbind(2 * X[, 1],
                                    (2 + sin(t*pi)*X[, 1]/1000)*X[, 1]) }

#Simulation is done in exactly the same manner as previously done. No change needed!
simulation <- ssa(X, pfun, v, tmin = 2, tmax = 10, nsim = 20, 
                    title = "Time-dependent Logistic Growth: Example2", 
                    xlab = "Time", ylab = "Individuals")

# -------------------------------------------
# 4. Lotka-Volterra

#Set seed
set.seed(3289650)

#Get initial parameters
params <- c(a = 3, b = 0.01, c = 2)
X <- matrix(c(100, 100), ncol = 2)

#Propensity function
pfun <- function(t, X, params){ cbind(params[1]*t*X[,1] + 1, 
                                            params[2]*X[,1]*X[,2], 
                                            params[3]*X[,2]) }
#Propensity score
v <- matrix(c(+1,-1,0,0,+1,-1),nrow=2,byrow=TRUE)

#Simulate
simulation <- ssa(X, pfun, v, params, 
                  title = "Example 4: Time-dependent Lotka-Volterra",
                  xlab = "Time", ylab = "Number of individuals")


