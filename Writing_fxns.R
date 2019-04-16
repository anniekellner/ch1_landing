rm(list = ls()) # clear environment

##################################
##### FUNCTIONS ##################
##################################

my_function = function(data, verbose=TRUE){
  x = lmer()
  y=predict()
  plot()
  return(x)
}
# arguments go inside parentheses 
# inside curly brackets -> instructions for what fxn will do
#runs through one line at a time
# last line is what gets returned (when sees return, exits out of fxn)
# can put return earlier, e.g. if (x>10) return(x)

#if you want to return multiple things, have to wrap in a list (make it a single entity)

# before write code, figure out what you're trying to do!!! --> write it down:

bar = function(x){
  #bar takes a number, returns its sqrt
  return(sqrt(x))
}                  

###################################################################################

# function reduces copying and pasting!
# especially good to use for plots


######################################################################################

### Using iris data - example!

# 1. calc mean of each group
# 2. plot
tmp = aggregate(Sepal.Length ~ Species, data=iris, FUN=mean)
dotchart(tmp$Sepal.Length, labels=tmp$Species)

# to make fxn, generalize! Need to make extensible

my_plot = function(x,y){
  #x = vector of data to summarize
  #y= grouping variable
  tmp = aggregate(x ~ y, FUN= mean)
  dotchart(tmp$x, labels = tmp$y, main = title)
}

my_plot(iris$Sepal.Length,iris$Species)

# DEBUGGING
my_plot = function(x,y){
  tmp = aggregate(x ~ y, FUN= mean)
  browser()
  dotchart(tmp$x, labels = tmp$y, main = title)
}
# browser takes console within function - can ask

# three dots
my_plot = function(x,y, method=mean, ...){
  #x = vector of data to summarize
  #y= grouping variable
  tmp = aggregate(x ~ y, FUN= mean)
  dotchart(tmp$x, labels = tmp$y, main = title, ...) #passes arguments to dotchart
}

my_plot(iris$Sepal.Width, iris$Species, main="BOb", pch = 16, col = "red") #lets user pass along arguments. Code is very flexible!

my_plot = function(x,y, method=mean, ...){
  tmp = aggregate(x ~ y, FUN= mean)
  dotchart(tmp$x, labels = tmp$y, main = title, ...)
  return(tmp) #returns tmp AND dotchart
}


# When referring script to a function you wrote, use source() at top
#library(raster)
#library(dplyr)
# source(./myfunctions.R)

# Packages are collections of functions