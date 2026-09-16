# Needed packages

checkpac <- function(package){if (!package %in% installed.packages())    
  install.packages(package)
  suppressPackageStartupMessages(library(package,character.only=TRUE))}

checkpac('nbconvertR')
checkpac('data.table')
checkpac('dplyr')
checkpac('multicool')
checkpac('microbenchmark')
checkpac('tidyverse')
checkpac('VennDiagram')
checkpac('gridExtra')
checkpac('prob')

#*****************************************************************
# credit: Tony Johnson 2016
#*****************************************************************
tabl <- function(object){
  x <- object@support
  prob <- d(object)(x)
  df <- data.frame(x,prob)
  return(df)
}
#*****************************************************************
# credit: Tony Johnson and Ian McCulloh 2016
#*****************************************************************
graph <- function(object, table=FALSE){
  n <- object@support
  name <- "Distribution"
  barplot(d(object)(n), main = name, xlab = "Event"
          , ylab = "Probability", names.arg = n )
  if (table==TRUE) {
    tabl(object)
  }
}
#*****************************************************************
# credit:https://rpubs.com/stephenmoore56/301283
#*****************************************************************
tossCoin = function(n=30, p=0.5) {
  
  # create a probability distribution, a vector of outcomes (H/T are coded using 0/1)
  # and their associated probabilities
  outcomes = c(0,1) # sample space
  probabilities = c(1-p,p)
  
  # create a random sample of n flips; this could also be done with
  # the rbinom() function, but sample() is perhaps more useful
  flips = sample(outcomes,n,replace=T,prob=probabilities)
  
  # now create a cumulative mean vector
  cum_sum = cumsum(flips)
  index = c(1:n)
  cum_mean = cum_sum / index
  
  # now combine the index, flips and cum_mean vectors
  # into a data frame and return it
  # return(data.frame(index,flips,cum_mean))
  return(data.frame(index,cum_mean))
}
#*****************************************************************
# credit:https://rpubs.com/stephenmoore56/301283
#*****************************************************************
ggplotCoinTosses = function(n=30, p=.50) {
  # visualize how cumulative average converges on p
  # roll the dice n times and calculate means
  trial1 = tossCoin(n,p)
  max_y = ceiling(max(trial1$cum_mean))
  if (max_y < .75) max_y = .75
  min_y = floor(min(trial1$cum_mean))
  if (min_y > .4) min_y = .4
  
  # calculate last mean and standard error
  last_mean = round(trial1$cum_mean[n],9)
  
  # plot the results together
  plot1 = ggplot(trial1, aes(x=index,y=cum_mean)) +
    geom_line(colour = "blue") +
    geom_abline(intercept=0.5,slope=0, color = 'red', size=.5) +
    theme(plot.title = element_text(size=rel(1.5)),
          panel.background = element_rect()) +
    labs(x = "n (number of tosses)",
         y = "Cumulative Average") +
    scale_y_continuous(limits = c(min_y, max_y)) +
    scale_x_continuous(trans = "log10",
                       breaks = trans_breaks("log10",function(x) 10^x),
                       labels = trans_format("log10",math_format(10^.x))) +
    annotate("text",
             label=paste("Cumulative mean =", last_mean,
                         "\nEV =",  p,
                         "\nSample size =", n),
             y=(max_y - .20),
             x=10^(log10(n)/2), colour="darkgreen") +
    annotate("text",
             label=paste("P(Heads) with Fair Coin = 0.50"),
             y=(max_y - .80),
             x=10^(log10(n)/2), colour="red")
  
  return(plot1)
}
#*****************************************************************
# credit: Tony Johnson 2016
#*****************************************************************

roll <- function(n) {
  mean(sample(die, size = n, replace = TRUE))
}

#*****************************************************************
# credit: https://rpubs.com/pythonjokeun/samplingdistribution
#*****************************************************************

plot_sample_means <- function(f_sample, n, m=300,title="Histogram", ...) {
  
  # define a vector to hold our sample means
  means <- double(0)
  
  # generate 300 samples of size n and store their means
  for(i in 1:m) means[i] = mean(f_sample(n,...))
  
  # scale sample means to plot against standard normal
  scaled_means <- scale(means)
  
  # set up a two panel plot
  par(mfrow=c(1,2))
  par(mar=c(5,2,5,1)+0.1)
  
  # plot histogram and density of scaled means
  hist(scaled_means, prob=T, col="light grey", border="grey", main=NULL, ylim=c(0,0.4))
  lines(density(scaled_means))
  
  # overlay the standard normal curve in blue for comparison
  curve(dnorm(x,0,1), -3, 3, col='blue', add=T)
  
  # adjust margins and draw the quantile-quantile plot
  par(mar=c(5,1,5,2)+0.1)
  qqnorm(means, main="")
  
  # return margins to normal and go back to one panel
  par(mar=c(5,4,4,2)+0.1)
  par(mfrow=c(1,1))
  
  # add a title
  par(omi=c(0,0,0.75,0))
  title(paste(title, ", n=", n, sep=""), outer=T)
  par(omi=c(0,0,0,0))
  
  # return unscaled means (without printing)
  return(invisible(means))
}
