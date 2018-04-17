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

rm(list= ls())

set.seed(963258)  # <-- to replicate data everytime you run this

N = 500 # Number of observations to generate


##
## Define covariates
## Feel free to fiddle with these values to get something more life-like
## rather than this text-book styled example

B.0 = 5  # intercept
B.1 = 1 # slope
noise = 10  # variance/ random variation of the model

x = runif(N, min = -10, max = 10) # generate covariate

##
## Generate data
y = B.0 + B.1*x + rnorm(N, mean = 0, sd = sqrt(noise))

##
## Visualise the data

dat = data.frame(x = x, y=y)

x11()  # plot the data only
ggplot(dat, aes(x=x, y=y)) + geom_point() + theme_bw() + ggtitle("Simulated data") +
  ylab("Response (y)") + xlab("Covariate (x)")


x11()  # plot the data OVERLAYED with a linear model
ggplot(dat, aes(x=x, y=y)) + geom_point() + theme_bw() + ggtitle("Simulated data") +
  ylab("Response (y)") + xlab("Covariate (x)") + 
  geom_smooth(fill = "red", alpha = 0.2,colour = "red", method = 'lm', formula = y~x, se=TRUE, level = 0.95)


sim.dat = list(dat=dat, B.0 = B.0, B.1 = B.1, noise = noise)

save(sim.dat, file = "Sim.Data.Rdata")
