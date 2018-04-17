###############################################
# R code to simulate simple lienar model with 
# one covariate.
#
# Model is 
#  y  = B.0 + B.1*x + noise 
#
# Tuesday 17.4.2018
# Marcela Cespedes


library(ggplot2)
#library(reshape2)

set.seed(963258)  # <-- to replicate data everytime you run this

N = 500 # Number of observations to generate


##
## Define covariates
## Feel free to fiddle with these values to get something more life-like
## rather than this text-book styled example

B.0 = 5  # intercept
B.1 = 1 # slope
noise = 1  # variance/ random variation of the model

x = runif(N, min = 0, max = 20) # generate covariate

##
## Generate data
y = B.0 + B.1*x + rnorm(N, mean = 0, sd = sqrt(noise))

##
## Visualise the data

dat = data.frame(x = x, y=y)

x11()
ggplot(dat, aes(x=x, y=y)) + geom_point() + theme_bw() + ggtitle("Simulated data") +
  ylab("Response (y)") + xlab("Covariate (x)")

save(dat, file = "Sim.Data.R")
