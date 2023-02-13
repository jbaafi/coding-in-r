# This code is based on the website https://rpubs.com/DistribEcology/880

# This function solves the model 
mod<-function(r=1,x_o=runif(1,0,1),N=100,burn_in=0,...)
{
  ############# Trace #############
  x<-array(dim=N)
  x[1]<-x_o
  for(i in 2:N)
    x[i]<-r*x[i-1]*(1-x[i-1])
  
  plot(x[(burn_in+1):N],type='l',xlab='t',ylab='x',...)
}

mod(r=3.5,x_o=0.4)


library(parallel)

# This does the bifurcation of the above model
bifurcation<-function(from=3,to=4,res=500,
                      x_o=runif(1,0,1),N=500,reps=500,cores=4)
{
  r_s<-seq(from=from,to=to,length.out=res)
  r<-numeric(res*reps)
  for(i in 1:res)
    r[((i-1)*reps+1):(i*reps)]<-r_s[i]
  
  x<-array(dim=N)
  
  iterate<-mclapply(1:(res*reps),
                    mc.cores=cores,
                    function(k){
                      x[1]<-runif(1,0,1)
                      for(i in 2:N)
                        x[i]<-r[k]*x[i-1]*(1-x[i-1])
                      
                      return(x[N])
                    })
  
  plot(r,iterate,pch=15,cex=0.1)
  
  return(cbind(r,iterate))
}

#warning: Even in parallel with 4 cores, this is by no means fast code!
bi<-bifurcation()
#png('chaos.png',width=1000,height=850)
#par(bg='white',col='blue',col.main='green',cex=1)
plot(bi,col='blue',xlab='R',ylab='n --> inf',main='',pch=15,cex=0.2)


# Another model
rmax <- 30
plot(-1, -1, xlim = c(0, rmax), ylim = c(0, 1000), xlab = "r", ylab = "N")
a <- 0.01
r <- seq(0, rmax, by = 0.1)

n <- 100

for (z in 1:length(r)) {
  xl <- vector()
  xl[1] <- 10
  for (i in 2:n) {
    
    xl[i] <- xl[i - 1] * r[z] * exp(-a * xl[i - 1])
    
  }
  uval <- unique(xl[40:n])
  points(rep(r[z], length(uval)), uval, cex = 0.1, pch = 19)
}

# Another way of doing the same plot
library(ggplot2)
rmax <- 30
out.df <- matrix(NA, ncol = 2, nrow = 0)
a <- 0.01
r <- seq(0, rmax, by = 0.01)
n <- 100

for (z in 1:length(r)) {
  
  xl <- vector()
  xl[1] <- 10
  for (i in 2:n) {
    
    xl[i] <- xl[i - 1] * r[z] * exp(-a * xl[i - 1])
    
  }
  uval <- unique(xl[40:n])
  ### Here is where we can save the output for ggplot
  out.df <- rbind(out.df, cbind(rep(r[z], length(uval)), uval))
}
out.df <- as.data.frame(out.df)
colnames(out.df) <- c("r", "N")
ggplot(out.df, aes(x = r, y = N)) + geom_point(size = 0.5)

# Another example for Lotka-Voltera model (continuous time)
rm(list = ls())
library(deSolve)

LotVmod <- function (Time, State, Pars) {
  with(as.list(c(State, Pars)), {
    dx = x*(alpha - beta*y)
    dy = -y*(gamma - delta*x)
    return(list(c(dx, dy)))
  })
}

n <- 100 # number of simulations
param.name <- "gamma" # choose parameter to perturb
param.seq <- seq(0,1,length = 50) # choose range of parameters

Pars <- c(alpha = 1, beta = .001, gamma = 1, delta = .001)
Time <- seq(0, 10, length = n)
State <- c(x = .5, y = .9)

param.index <- which(param.name == names(Pars))
out <- list()
for (i in 1:length(param.seq))
  out[[i]] <- matrix(0, n, length(State))

for (i in 1:length(param.seq)) {
  # set params
  Pars.loop <- Pars
  Pars.loop[param.index] <- param.seq[i]
  # converge
  init <- ode(State, Time, LotVmod, Pars.loop)
  # get converged points
  out[[i]] <- ode(init[n,-1], Time, LotVmod, Pars.loop)[,-1]
}

range.lim <- lapply(out, function(x) apply(x, 2, range))
range.lim <- apply(do.call("rbind", range.lim), 2, range)
plot.variable <- "x" # choose which variable to show
plot(0, 0, pch = "", xlab = param.name, ylab = plot.variable,
     xlim = range(param.seq), ylim = range.lim[,plot.variable])
for (i in 1:length(param.seq)) {
  points(rep(param.seq[i], n), out[[i]][,plot.variable])
}



# Example of using the package for producing bifurcation diagrams
library(deBif)

if(interactive()){
  # The initial state of the system has to be specified as a named vector of state values.
  state <- c(R=1, N=0.01)
  # Parameters has to be specified as a named vector of parameters.
  parms <- c(r=1, K=1, a=1, c=1, delta=0.5)
  # The model has to be specified as a function that returns
  # the derivatives as a list.
  model <- function(t, state, parms) {
    with(as.list(c(state,parms)), {
      dR <- r*R*(1 - R/K) - a*R*N
      dN <- c*a*R*N - delta*N
      # The order of the derivatives in the returned list has to be
      # identical to the order of the state variables contained in
      # the argument "state"
      return(list(c(dR, dN)))
    })
  }
  phaseplane(model, state, parms, saveplotas="pdf")
}


if(interactive()){
  # The initial state of the system has to be specified as a named vector of state values.
  state <- c(R=1, N=0.01)
  # Parameters has to be specified as a named vector of parameters.
  parms <- c(r=1, K=1, a=1, c=1, delta=0.5)
  # The model has to be specified as a function that returns
  # the derivatives as a list.
  model <- function(t, state, parms) {
    with(as.list(c(state,parms)), {
      dR <- r*R*(1 - R/K) - a*R*N
      dN <- c*a*R*N - delta*N
      # The order of the derivatives in the returned list has to be
      # identical to the order of the state variables contained in
      # the argument "state"
      return(list(c(dR, dN)))
    })
  }
  bifurcation(model, state, parms)
}

# -----------------------------------------------------------
# The system will still run without the interactive function

# The initial state of the system has to be
# specified as a named vector of state values.
state <- c(R = 0.05, C = 0.1)

# Parameters has to be specified as a named vector of parameters.
parms <- c(r = 0.5, K = 0.1, a = 5.0, h = 3.0, eps = 0.5, mu = 0.05)

# The model has to be specified as a function that returns
# the derivatives as a list. You can adapt the body below
# to represent your model
rosenzweig <- function(t, state, parms) {
  with(as.list(c(state,parms)), {
    dR = r*R*(1 - R/K) - a*R*C/(1 + a*h*R)
    dC = eps*a*R*C/(1 + a*h*R) - mu*C
    # The order of the derivatives in the returned list has to be
    # identical to the order of the state variables contained in
    # the argument `state`
    return(list(c(dR, dC)))
  })
}

bifurcation(rosenzweig, state, parms)



