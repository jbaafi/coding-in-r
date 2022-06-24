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

