##############################################
# Estimate model via the classical approach
#
# Model is 
#  y  = B.0 + B.1*x + noise 
#
# in this model, there are three parameters to estimate (B.0, B.1 and noise)


library(ggplot2)

rm(list = ls())
set.seed(963258)  # <-- to replicate results everytime you run this

## load data - pretend that all we see is the response and covariate
## we wish to recover the values for B.0, B.1 and noise
load("Sim.Data.Rdata")
str(sim.dat)

dat<- sim.dat$dat
head(dat)

# fit lm() model
mod<- lm(y~x, data = dat)

summary(mod)
summary(mod)$sigma^2  # <-- noise (s2) estimate

confint(mod, level = 0.95)



## summary of model output




### fit model output

x11()  # plot the data OVERLAYED with a linear model
ggplot(dat, aes(x=x, y=y)) + geom_point() + theme_bw() + ggtitle("Classical lm() model output") +
  ylab("Response (y)") + xlab("Covariate (x)") + 
  geom_smooth(fill = "red", alpha = 0.2,colour = "red", method = 'lm', formula = y~x, se=TRUE, level = 0.95)

