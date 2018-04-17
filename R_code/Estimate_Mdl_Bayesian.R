#############################################
# Bayesian approach to estimate the linear model
# 
# Model is 
#  y  = B.0 + B.1*x + noise 
#
# in this model, there are three parameters to estimate (B.0, B.1 and noise)
#
# Recall: the priors are listed in Visualise_Priors.R

library(invgamma)
library(mvtnorm)
library(scatterplot3d)
library(ggplot2)

rm(list = ls())
set.seed(963258)  # <-- to replicate results everytime you run this

## load data - pretend that all we see is the response and covariate
## we wish to recover the values for B.0, B.1 and noise
load("Sim.Data.Rdata")
str(sim.dat)


##
## For this simple example we will use the Gibbs sampler to draw 
## posterior distributions. 
## The Gibbs sampler is one form of MCMC. It is desirable to use because 
## it directly samples from the conditional posterior distribution and NO
## tuning values are requried. You just code it and off it goes.
##
## in order to use it,  we need to have the full conditional distributions
## i.e p(parameter | other parameter) in known form (such as a univariate normal)
##

## How long with the MCMC chain run?
M = 1000

## no. of observations in the data
N = length(sim.dat$dat$x)

X = cbind(rep(1, N), sim.dat$dat$x)  # Create covariate matrix
y = sim.dat$dat$y # response vector


## Define place to store MCMC chains
mcmc<- matrix(0, ncol = 3, nrow = M)

## initialis starting values
mcmc[1,]<- c(3, 0.5, 2)

# Column 1 of mcmc.chain will be the chain for B.0
# Column 2 of mcmc.chain will be the chain for B.1
# column 3 of mcmc.chain will be the chain for noise

# Hint: This works better if you give it a good place to start from
#        look at a plot of the data to 'guess-timate' a good starting point

## Define the priors
prior.b.mu = c(0,0)
prior.b.Sigma = matrix(c(100, 0, 0, 100), 2,2)
inv.prior.b.Sigma = solve(prior.b.Sigma)

prior.noise = 0.1

        ##################
        ## start MCMC   ##
        ##################


for(m in 2:M){
  
  # ***************************************************************
  # Draw posterior sample for B = [B.0, B.1]
  Sigma.b<- solve(1/mcmc[m-1, 3]*t(X)%*%X + inv.prior.b.Sigma)
  mean.b<- (1/mcmc[m-1, 3]*t(y)%*%X)%*%Sigma.b
  
  post.B <- rmvnorm(1, mean = mean.b, sigma = Sigma.b)  
  #post.B  # <-- update for vector B = [B.0, B.1]
  
  mcmc[m, 1:2]<- post.B
  
  #post.B<- c(sim.dat$B.0, sim.dat$B.1)
  #mcmc[m, 1:2]<- c(sim.dat$B.0, sim.dat$B.1)
  # ****************************************************************
  
  
  # ****************************************************************
  # Draw posterior sample for noise (sigma^2)
  
  shape.s2 <- prior.noise + N/2
  
  t1<- (y - post.B%*%t(X))^2
  #t<- (y - rep(1, N)*post.B[1] - post.B[2]*X[,2])^2
  scale.s2 <- prior.noise + sum(t1)/2
  scale.s2
  #scale.s2 <- prior.noise/2*(y - post.B%*%t(X))%*%t(y - post.B%*%t(X))
  
  
  # I think there is something wrong with the implementation of the rinvgamma()
  #post.s2 <- rinvgamma(1, shape = shape.s2, rate = scale.s2)  # <-- this works!
  #                                                           but I remember working on the scale, NOT rate
  
  post.s2 <- 1/rgamma(1, shape = shape.s2, rate = scale.s2)
  post.s2
  
  mcmc[m,3]<- post.s2
}

          ##################
          ## End   MCMC   ##
          ##################


head(mcmc)
tail(mcmc)

str(sim.dat)


#####################################
#####################################

# ** to include MCMC diagnositics


