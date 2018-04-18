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

confint(mod, level = 0.95)  # CI values for B.0 and B.1

## summary of model output
op<- data.frame(parameters = c("B.0", "B.1", "s2"), 
           est = round(c(summary(mod)$coefficient[1,1], summary(mod)$coefficient[2,1], summary(mod)$sigma^2),2),
           low.ci = c(round(c(confint(mod, level = 0.95)[1,1], confint(mod, level = 0.95)[2,1]),2), "-")  ,
           high.ci = c(round(c(confint(mod, level = 0.95)[1,2], confint(mod, level = 0.95)[2,2]),2), "-"),
           Solution = c(sim.dat$B.0, sim.dat$B.1, sim.dat$noise) )

op

### fit model output

x11()  # plot the data OVERLAYED with a linear model
ggplot(dat, aes(x=x, y=y)) + geom_point() + theme_bw() + ggtitle("Classical lm() model output") +
  ylab("Response (y)") + xlab("Covariate (x)") + 
  geom_smooth(fill = "red", alpha = 0.2,colour = "red", method = 'lm', formula = y~x, se=TRUE, level = 0.95)



# visualise the estimates along the number line
n.B0<- ggplot(data.frame(x = c(summary(mod)$coefficient[1,1], confint(mod, level = 0.95)[1,1],confint(mod, level = 0.95)[1,2]), y = rep(0, 3)), 
              aes(x=x, y=y)) + geom_point(size = 3, colour = "red") +
       geom_hline(yintercept = 0, colour = "black") + theme_bw() + ggtitle("Point estimate B.0") + ylab("Density") + xlab("Real line")+
  ylim(c(-0.1, 0.5))

n.B1<- ggplot(data.frame(x = c(summary(mod)$coefficient[2,1],confint(mod, level = 0.95)[2,1], confint(mod, level = 0.95)[2,2]),y = rep(0, 3)), 
              aes(x=x, y=y)) + geom_point(size = 3, colour = "red")+
  geom_hline(yintercept = 0, colour = "black") + theme_bw() + ggtitle("Point estimate B.1")+ ylab("Density") + xlab("Real line")+
  ylim(c(-0.1, 0.5))

n.s2<- ggplot(data.frame(x = summary(mod)$sigma^2,y = 0), aes(x=x, y=y)) + geom_point(size = 3, colour = "red")+
  geom_hline(yintercept = 0, colour = "black") + theme_bw() + ggtitle("Point estimate noise (s2)")+ ylab("Density") + xlab("Real line")+
  ylim(c(-0.1, 0.5))

source("multiplot.r")

x11()
multiplot(n.B0, n.B1, n.s2, cols = 3)