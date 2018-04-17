#######################################################
# Fiddle with animations code snippets


rm(list = ls())

# from https://gist.github.com/thomasp85/88d6e7883883315314f341d2207122a1
library(ggplot2)
library(tweenr) # Available on CRAN
library(ggforce) # Available on CRAN

#devtools::install_github("dgrtwo/gganimate")
library(gganimate) # Install from dgrtwo/gganimate

set.seed(2)
x <- sample(9,20, prob=c(1,2,3,4,5,4,3,2,1), replace=T)
df <- data.frame(x = x, y = 15)
dfs <- list(df)

for(i in seq_len(nrow(df))) {
  dftemp <- tail(dfs, 1)
  dftemp[[1]]$y[i] <- sum(dftemp[[1]]$x[seq_len(i)] == dftemp[[1]]$x[i])
  dfs <- append(dfs, dftemp)
}

dfs <- append(dfs, dfs[rep(length(dfs), 3)])

dft <- tween_states(dfs, 10, 1, 'cubic-in', 200)
head(dft,20)
class(dft)

dft$y <- dft$y - 0.5
dft <- dft[dft$y != 14.5, ]
dft$type <- 'Animate'

dfh <- data.frame(x=x, type = 'Histogram')
head(dfh)
tail(dfh)

p <- ggplot(dft) + 
  geom_circle(aes(x0=x, y0=y, r=0.5, frame = .frame), n=20, fill = 'steelblue') + 
  geom_histogram(aes(x=x), data = dfh, fill = 'forestgreen', color = 'black', binwidth = 1) + 
  coord_fixed(ylim = c(0, 13.5)) + 
  theme_bw() + 
  facet_grid(.~type)

animation::ani.options(interval = 1/20)

# finally got this example working!!!
# Turns out that I needed to have both R and ImageMagick installed not in 'Program Files' but in a directory
# with no spaces!!!
Sys.getenv("PATH")
Sys.setenv(PATH = paste("C:\\Users\\ces007\\Documents\\ImageMagick-7.0.7-28-portable-Q16-x64", Sys.getenv("PATH"), sep = ";"))

gganimate(p,'hist_ex.gif', title_frame = FALSE)
#gganimate(p, 'hist_ex.gif', title_frame = FALSE)




################################
# example from ggaminate
aq <- airquality
aq$date <- as.Date(paste(1973, aq$Month, aq$Day, sep = "-"))
head(aq)
p2 <- ggplot(aq, aes(date, Temp, frame = Month, cumulative = TRUE)) +
  geom_line()

gganimate(p2, "airQuality.gif",title_frame = FALSE)  # <-- this is a crap example
















