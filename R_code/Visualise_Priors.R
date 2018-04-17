#############################################
# Fit a Bayesian linear model
# 
# Model is 
#  y  = B.0 + B.1*x + noise 
#
#

library(invgamma)
#library(MASS)
library(mvtnorm)
library(scatterplot3d)
library(ggplot2)

rm(list = ls())

set.seed(963258)  # <-- to replicate results everytime you run this

## load data
load("Sim.Data.Rdata")
str(sim.dat)

##
## Visualise priors - dot/ red line denotes solution

########################################
# for (B.0, B.1)  **********************
x1<- x2<- seq(from = -20, to = 20, length = 51)
dens<- matrix(dmvnorm(expand.grid(x1, x2),
                      sigma = matrix(c(100, 0, 0, 100), 2,2)),
                      ncol = length(x1) )

x11()
s3d <- scatterplot3d(x1, x2,
                     seq(min(dens), max(dens), length = length(x1)),
                     type = "n", grid = FALSE, angle = 210, 
                     zlab = expression(f(beta[0], beta[1])),
                     xlab = expression(beta[0]), ylab = expression(beta[1]),
                     main = expression(paste("Bivariate normal distribution: prior for (", beta[0], beta[1], ")", sep = "")) )
             
# insert MVN grid
for(i in length(x1):1)
  s3d$points3d(rep(x1[i], length(x2)), x2, dens[i,], type = "l")
for(i in length(x2):1)
  s3d$points3d(x1, rep(x2[i], length(x1)), dens[,i], type = "l")

# include solution values for (B.0, B.1)
s3d$points3d(sim.dat$B.0, sim.dat$B.1, 0, pch = 18, color = "red")

#######################################
# for noise (sigma^2)  ****************
x<- seq(from= 0.0001, to = 100, by = 0.01)
# my math expressions I work with the scale = 1/rate
scale = 0.1
y<- dinvgamma(x, shape = 0.1, rate= 1/scale)  

x11()
ggplot(data.frame(x=x, y=y), aes(x=x, y=y)) + geom_line() + theme_bw() + 
  ggtitle("Inverse gamma distribution: prior for noise") + geom_vline(xintercept = sim.dat$noise, colour = "red")
