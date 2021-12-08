# Title: GILLESPIE SSA WITH TIME-DEPENDENT PARAMETERS
# Date: 01/12/2021

# Clear workspace
rm(list = ls())

# Working directory
#setwd("/Users/jbaafi/Documents/coding-in-r/R-worked-examples")

# Load packages
library(GillespieSSA)
library(ssar)
library(ggplot2)

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
pfun <- function(t,X,params){ cbind(2 * X[, 1],
                                    (1 + 1*X[, 1]/1000)*X[, 1]) }

#The model runs automatically from 0 to 1 conducting 10 simulations and generating a plot.
simulation <- ssa(X, pfun, v,  nsim = 1)

# The time for the simulation and number of simulations can be specified:
tmin       <- 0
tmax       <- 10
nsim       <- 1

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
                    title = "Time-dependent Logistic Growth", 
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

# Save the file
simulation <- ssa(X, pfun, v, params, keep.file = TRUE, fname ="My_simulation.txt",
                  plot.sim = FALSE)

# You can read the file with the read.table function:
sim <- read.table("My_simulation.txt",  header = TRUE)

# Plot with ggplot2
ggplot(data = sim, aes(x = Time, group = as.factor(Simulation))) +
  geom_line(aes(y = Var1, color = "Prey")) +
  geom_line(aes(y = Var2, color = "Predator")) +
  ggtitle("Lotka Volterra with ggplot2") + 
  xlab("Time") + ylab("Individuals") +
  scale_color_manual("Creature", 
                     values = c("Prey" = "deepskyblue4","Predator" = "tomato3"))

# ----------------------------------------------------------
# 5. Lotka-Volterra with random time-dependent parameters
# This is almost the same Lotka-Volterra model; however in this case the parameters 
# a and b are random variables.

#Set seed
set.seed(3289650)

#Get initial parameters
params <- c(amu = 3, asd = 0.01, bmin = 0.001, bmax = 0.015, c = 2)
X <- matrix(c(100, 100), ncol = 2)

#Propensity function
pfun <- function(t, X, params){ cbind(rnorm(1,params[1], params[2])*X[,1] + 1, 
                                            runif(1,params[3],params[4])*X[,1]*X[,2], 
                                            params[5]*X[,2]) }

#Propensity score
v <- matrix(c(+1,-1,0,0,+1,-1),nrow=2,byrow=TRUE)

#Simulate
simulation <- ssa(X, pfun, v, params, 
                  title = "Lotka-Volterra with random variables",
                  xlab = "Time", ylab = "Number of individuals")


#Notice that the random variables in the model can also be time-dependent:
#Propensity function
pfun <- function(t, X, params){ 
    cbind(rnorm(1,t + params[1], params[2])*X[,1] + 1, 
          runif(1,params[3],params[4])*X[,1]*X[,2], params[5]*X[,2]) }

#Simulate
simulation <- ssa(X, pfun, v, params, 
                  title = "Example 5: Lotka-Volterra with time-dependent random variables",
                  xlab = "Time", ylab = "Number of individuals")

# ---------------------------------------------
#EXAMPLE 6
#---------------------------------------------
#Time dependent SIS model
set.seed(123)

#Initial parameters
k <- 24576.5529836797
delta <- 0.0591113454895868 + 0.208953907151055
gamma_ct <-  0.391237630231631
params <- c(k = k, delta = delta, gamma_ct = gamma_ct)
X <- matrix(c(S = 1000, I = 40), ncol = 2)
pfun <- function(t, X, params){
  
#Value to return
matreturn <- matrix(NA, nrow = length(t), ncol = 6)
  
#Create birth function
lambda <- function(t){ return(4.328e-4 - (2.538e-7)*t - 
                                      (3.189e-7)*sin(2 * t * pi/52) - 
                                      (3.812e-7)*cos(2 * t * pi/52) ) }
  
#Create death function
 mu <- function(t){ return(9.683e-5 + (1.828e-8)*t + 
                                      (2.095e-6)*sin(2 * t * pi/52) - 
                                      (8.749e-6)*cos(2 * t * pi/52))}
  
#Create infectives function
beta_fun <- function(t){ return( 0.479120824267286 + 
                                       0.423263042762498*sin(-2.82494252560096 + 2*t*pi/52) )}
  
#Estimate values
matreturn[,1] <- lambda(t)*(X[,1] + X[,2])
matreturn[,2] <- mu(t)*X[,1]
matreturn[,3] <- beta_fun(t)*X[,1]*X[,2]/(1 + params[1]*X[,2])
matreturn[,4] <- mu(t)*X[,2]
matreturn[,5] <- params[2]*X[,2]
matreturn[,6] <- params[3]*X[,2]

#Return
return(matreturn)
  
}
v <- matrix(c(1,-1, -1, 0, 0, 1, 0, 0, 1, -1, -1, -1), nrow = 2, byrow = TRUE)
tmin <- 0
tmax <- 0.5
nsim <- 1

#Simulate the values
simulation <- ssa(X, pfun, v, params, tmin, tmax, nsim = nsim, print.time = FALSE, 
                  plot.sim = TRUE, kthsave = 1)
