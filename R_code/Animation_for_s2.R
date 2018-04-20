####################################################
# Animation of posterior distributions being 'drawn'
# along the MCMC iteration

##
## the rendering seems to be taking ages!!!

## good ggplot animation comparison  methods
# https://www.r-graph-gallery.com/animation/


library(ggplot2)
library(tweenr) # Available on CRAN
library(ggforce)

#devtools::install_github("dgrtwo/gganimate")
library(gganimate) # Install from dgrtwo/gganimate


rm(list = ls())

load("MCMC_results.Rdata")
str(full.results)

dat<- full.results$diagnostics

dim(dat)  # use only half the chains
dat<- dat[1:floor(dim(dat)[1]/5),]

dim(dat)

## make a historam of B.0 - I need to get the x (bin) values
#x11()
#ggplot(dat, aes(x = B.0)) + geom_histogram()
#op<- hist(dat$B.0, breaks = 20)
#str(op)
#x11()
#plot(op, main = "No rounding")

#####################
##################### Select parameter do do animation on !!!!
x11()
op<- hist(round(dat$s2,2), breaks = 20)  # leave it rounded to two decimal places
str(op)
plot(op, main = "Data rounded to one decimal place")

hist.dat<- data.frame(x = op$breaks[-20], y =op$counts)
hist.dat

### convert this into a single vector
vec.st<- c(100)

for(i in 1:length(op$counts)){
  vec.st<- c(vec.st, rep(hist.dat[i,1], hist.dat[i,2]))
}
vec.st<- vec.st[-1]

# make the order random - for a better animation
vec.st<- vec.st[sample(1:length(vec.st), length(vec.st), replace=FALSE)]
head(vec.st,50)



### my attempt to set up the data for the animation of B.0
#df.a<- data.frame(x = round(dat$s2,2), y = 150)
df.a<- data.frame(x = vec.st, y = 60)
dfs.a<- list(df.a)

for(i in seq_len(nrow(df.a))) {
  dftemp.a <- tail(dfs.a, 1)
  
  dftemp.a[[1]]$y[i] <- sum(dftemp.a[[1]]$x[seq_len(i)] == dftemp.a[[1]]$x[i])
  dfs.a <- append(dfs.a, dftemp.a)
}

dfs.aa <- append(dfs.a, dfs.a[rep(length(dfs.a), 3)])

# how to determine the length of the transition of each state? tween_length??
dft.a <- tween_states(data=dfs.aa, tweenlength=10, 
                      statelength=1, ease='cubic-in', nframes=200) # <-- this turns dfs list into a data.frame


head(dft.a)
tail(dft.a)

dim(dft.a)  # rounding did not reduce the length of this. This has 4.5 million frames!!!!


#dft.a$y <- dft$y - 0.5
unique(dft.a$y)
dft.a <- dft.a[dft.a$y != 60, ] # <-- this removes the extra data of the ball that hovers at y = 14.5  (top margin)
dim(dft.a)  
dft.a$type <- 'Animation.of.MCMC.iteration'

#dfh.a<- data.frame(x =round(dat$s2,2)) # <-- not the actual histogram
dfh.a<- data.frame(x = vec.st)
dfh.a$type = "Histogram.of.Posterior.Draws"

#x11()
#ggplot(dfh.a, aes(x=x)) +geom_histogram(binwidth = 0.1) + xlim(c(0,20)) +ylim(c(0,60))


## my attempt at the animation
p.a <- ggplot(dft.a) + 
  geom_circle(aes(x0=x, y0=y, r=0.45, frame = .frame), n=length(vec.st), # <-- n: is the number of points on the generated path
              fill = 'steelblue') + 
  coord_fixed(ylim = c(0, 42), xlim = c(0,20)) + 
  theme_bw() + ggtitle("MCMC animation for noise (s2) posterior draws") +
  xlab("Real line") + ylab("Counts") +
  geom_histogram(aes(x=x), data = dfh.a, fill = 'forestgreen', 
                 color = 'black', binwidth = 0.1, alpha = 0.4) + 
  facet_grid(.~type)
   

animation::ani.options(interval = 0.5) # I think this is the speed of the animation

#Sys.getenv("PATH")
#Sys.setenv(PATH = paste("C:\\Users\\ces007\\Documents\\ImageMagick-7.0.7-28-portable-Q16-x64", Sys.getenv("PATH"), sep = ";"))

ptm <- proc.time()
# with hundreds of thousands of frames ... this takes a long time (> 5 min) to run
gganimate(p.a,'attempt4_s2.gif', title_frame = FALSE)
(proc.time() - ptm)/60  # time in minutes


## 64,000 frames took ~3 min
## then hopefully 253,000 should take 12 min ???








set.seed(2)
# x is the data
x <- sample(9,20, prob=c(1,2,3,4,5,4,3,2,1), replace=T)
df <- data.frame(x = x, y = 15)  # <-- why 15?  this is a higher value than the total number of counts (hidden top margin)
dfs <- list(df)

for(i in seq_len(nrow(df))) {
  dftemp <- tail(dfs, 1)
  
  dftemp[[1]]$y[i] <- sum(dftemp[[1]]$x[seq_len(i)] == dftemp[[1]]$x[i])
  dfs <- append(dfs, dftemp)
}

dfs <- append(dfs, dfs[rep(length(dfs), 3)])

# I think the above is done to set up the frames one by one
# but in a much quicker way
# *********************

dft <- tween_states(data=dfs, tweenlength=10, statelength=1, ease='cubic-in', nframes=200) # <-- this turns dfs list into a data.frame
# this outputs the .frame variable
# so the loop above was used to create the additional data in order to generate the animation
# this tween_states  - creates the additional data and puts it into a data frame ready for gganimation 



####  only the animation - my fiddle
p <- ggplot(dft) + 
  geom_circle(aes(x0=x, y0=y, r=0.5, frame = .frame), n=20, # <-- wonder if 'n' has to do with the interval
              fill = 'steelblue') + 
  #geom_histogram(aes(x=x), data = dfh, fill = 'forestgreen', color = 'black', binwidth = 1) + 
  #coord_fixed(ylim = c(0, 13.5)) + 
  coord_fixed(ylim = c(0, 15)) + 
  theme_bw() + ggtitle("Fiddle with histogram") +
  xlab("Posterior draws for B.0") + ylab("Counts")
