# Title: GILLESPIE SSA WITH TIME-DEPENDENT PARAMETERS 
# Date: 01/12/2021

# Clear workspace
rm(list = ls())

# Working directory
setwd("/Users/jbaafi/Documents/coding-in-r/R-worked-examples")

# Load packages
library(GillespieSSA)
library(ssar)

# GILLESPIE SSA WITH TIME-DEPENDENT PARAMETERS 
#EXAMPLE 1 
#------------------------
#Logistic Growth
set.seed(123)
params     <- c(b=2, d=1, k=1000)
X          <- matrix(c(N=500), nrow = 1)
pfun       <- function(t,X,params){ cbind(params[1] * X[,1], 
                                          (params[2] + (params[1]-params[2])*X[,1]/params[3])*X[,1]) }
v          <- matrix( c(+1, -1),ncol=2)
tmin       <- 0
tmax       <- 1
nsim       <- 5
simulation <- ssa(xinit=X, pfun=pfun, v=v, params=params, tmin=tmin, tmax=tmax, nsim=nsim,
                  title = "Logistic Growth example", xlab = "Time", ylab = "N")


#EXAMPLE 2
#------------------------
#Time dependent logistic growth
set.seed(123)
params     <- c(b=2, d=1, k=1000)
X          <- matrix(c(N=500), nrow = 1)
pfun       <- function(t,X,params){ cbind(params[1] *(1 + sin(t))* X[,1], 
                                          (params[2] + (params[1]-params[2])*X[,1]/params[3])*X[,1]) }
v          <- matrix( c(+1, -1),ncol=2)
tmin       <- 0
tmax       <- 1
nsim       <- 5
simulation <- ssa(X = X, pfun = pfun, v = v, params = params, tmin = tmin, tmax = tmax, nsim = nsim, 
                  title = "Time dependent Logistic Growth example", 
                  xlab = "Time", ylab = "N")

