#Title: Solve ODEs
#Date created: June 19, 2022

# Clear workspace
rm(list = ls())

# Set working directory
#setwd("")

# Load packages
pacman::p_load(deSolve, tidyverse)

# Define the model
basic.pop.model <- function (t, x, params) {
  ## first extract the state variables
  J <- x[1]
  A <- x[2]
  ## now extract the parameters
  b <- params["b"]
  phi_A <- params["phi_A"]
  phi_J <- params["phi_J"]
  mu_J <- params["mu_J"]
  mu_A <- params["mu_A"]
  ## now code the model equations
  dJdt <- b*phi_A*A - (phi_J+mu_J)*J
  dAdt <- phi_J*J - mu_A*A
  ## combine results into a single vector
  dxdt <- c(dJdt,dAdt)
  ## return result as a list!
  list(dxdt)
}

# values for the parameters using Lutambi et al, 2013
params <- c(b=10, phi_A=3, phi_J=0.121, mu_J=0.143, mu_A=0.032)

# the times at which we want solutions 
times <- seq(from=0, to=365)

# the initial conditions
xstart <- c(J=100, A=10)

# we compute a model trajectory and store the result in a data-frame:
ode(
  func=basic.pop.model,
  y=xstart,
  times=times,
  parms=params
) %>%
  as.data.frame() -> out

#  plot the results
# out %>%
#   gather(variable,value,-time) %>%
#   ggplot(aes(x=time,y=value,color=variable))+
#   geom_line(size=2)+
#   theme_classic()+
#   labs(x='time (days)',y='number of individuals')

plot(out$time, out$J, type = "l", col = "red")
lines(out$time, out$A, type = "l", col = "blue", lwd = 1)


# -------------------------------------------------------------
closed.sir.model <- function (t, x, params) {
  ## first extract the state variables
  E <- x[1]
  A <- x[2]
  ## now extract the parameters
  beta <- params["beta"]
  gamma <- params["gamma"]
  ## now code the model equations
  dEdt <- beta*A-gamma*E
  dAdt <- gamma*E
  ## combine results into a single vector
  dxdt <- c(dEdt,dAdt)
  ## return result as a list!
  list(dxdt)
}

params <- c(beta=1,gamma=0.05) # per year
#params <- c(beta=1.096,gamma=0.0769)

#times <- seq(from=0,to=60/365,by=1/365/4) # per year
times <- seq(from=0,to=50,by=0.01) # returns a sequence
xstart <- c(E=0.00,A=100) 

out <- as.data.frame(
  ode(
    func=closed.sir.model,
    y=xstart,
    times=times,
    parms=params
  )
)

plot(E ~ time,data=out,type='l')
lines(A~time, data=out, col = "blue")


#define plot area as three rows and one column
par(mfrow = c(2, 1), mar = c(3, 2, 1, 3))    

#create plots
plot(E~time, data = out, pch=19, col='red', type = "l")
plot(A~time, data = out, pch=19, col='blue', type = "l")




