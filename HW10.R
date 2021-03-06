# -----------------------------------
# Homework 10 for BIO381
# 14 Apr 2021
# EMD
# -----------------------------------
#

# Question 1 ------------------------------

#Setup global variables:
count <- 0

x <- c(rep(0:1, 1000))

# -----------------------------------
# FUNCTION counter
# description: counts the number of 0s in a vector
# inputs: x
# outputs: num
#####################################
counter <- function(x) {
  
  for (i in seq_along(x)) {
    if (x[i] == 0) {
      count <- count + 1 
    }
  }
  return(count)
}
# end of counter
# -----------------------------------
counter(x)

# Question 2 ------------------------------

#Preliminaries

counter2 <- 0
x2 <- c(rep(0:1, 1000))

# code

print(counter2 <- length(x2[x2 == 0]))


# Question 3 ------------------------------

# -----------------------------------
# FUNCTION matrix
# description: this function creates a matrix with two column and row numbers specified
# inputs: c and r
# outputs: outputs a matrix
#####################################
mat_make <- function(c = 4, r = 5) {
  
  mat <- base::matrix(data = NA, ncol = c, nrow = r)
  for(i in 1:nrow(mat)) {
    for(j in 1:ncol(mat)) {
      mat[i,j] <- i*j
    }
  }
  
  return(mat)
  
} # end of matrix

mat_out <- mat_make()

# -----------------------------------



# Question 4 ------------------------------

# Preliminaries
library(TeachingDemos)
char2seed('StarTrek')
library(ggplot2)

# Code

# Data analysis

# -----------------------------------
# FUNCTION read_data
# description: read in (or generate) data set for analysis
# inputs: file name (or nothing, as in this demo)
# outputs: 3 column data frame of observed data
#####################################
read_data <- function(z=NULL) {
  if(is.null(z)){
    x_obs <- 1:20
    y_obs <- x_obs + 10*rnorm(20)
    
  } 
  df <- read.table(file=z,
                   header=TRUE, sep = ',')
  
  
  return(df)
  
} # end of read_data
# -----------------------------------



# -----------------------------------
# FUNCTION get_metric
# description: calculate metric for randomization test
# inputs: 2-column data frame for regression
# outputs: regression slope
#####################################
get_metric <- function(z=NULL) {
  if(is.null(z)){
    x_obs <- 1:20
    y_obs <- x_obs + 10*rnorm(20)
    z <- data.frame(ID=seq_ALONG(x_obs),
                    x_obs,
                    y_obs)
  }
  
  . <- glm(family = 'binomial', z[,1]~z[,2])
  . <- summary(.)
  . <- .$coefficients[2,1]
  slope <- .
  
  
  
  return(slope)
  
} # end of get_metric
# -----------------------------------



# -----------------------------------
# FUNCTION shuffle_data
# description: randomize data for regression analysis
# inputs: 2 column data frame (xvar, yvar)
# outputs: 2 column data frame (xvar, yvar)
#####################################
shuffle_data <- function(z=NULL) {
  if(is.null(z)) {
    x_obs <- 1:20
    y_obs <- x_obs + 10*rnorm(20)
    z <- data.frame(ID=seq_along(x_obs),
                    x_obs,
                    y_obs)}
  z[,2] <- sample(z[,2])
  
  
  
  
  return(z)
  
} # end of shuffle_data
# -----------------------------------


# -----------------------------------
# FUNCTION get_pval
# description: calulate p value from simulation
# inputs: list of observed metric and vector of simulated metrics
# outputs: lower, and upper tail probability value
#####################################
get_pval <- function(z=NULL) {
  if(is.null(z)){
    z <- list(rnorm(1),rnorm(1000)) }
  p_lower <- mean(z[[2]]<=z[[1]])
  p_upper <- mean(z[[2]]>=z[[1]])
  
  return(c(pL=p_lower,pU=p_upper))
  
  
} # end of get_pval
# -----------------------------------


# -----------------------------------
# FUNCTION plot_ran_test
# description: create a ggplot of histogram of simulated values
# inputs: list of observed metric and vector simulated metrics
# outputs: saved ggplot graph
#####################################
plot_ran_test <- function(z=NULL){
  if(is.null(z)){
    z <- list(rnorm(1),rnorm(1000))
  }
  
  df <- data.frame(ID=seq_along(z[[2]]), sim_x=z[[2]])
  
  
  
  p1 <- ggplot2::ggplot(data=df, mapping=aes(x=sim_x))
  p2 <- p1 + geom_histogram(mapping=aes(fill=I("goldenrod"), color=I("black"))) + geom_vline(aes(xintercept=z[[1]], col="blue"))
  plot(p2)
  
  
  return("Checking...plot_ran_test")
  
} # end of plot_ran_test
# -----------------------------------
#plot_ran_test()


n_sim <- 1000 # number of simulated data sets
x_sim <- rep(NA,n_sim) # set up empty vector for simulated slopes
df <- read_data('Real_data_true.csv') # get data
x_obs <- get_metric(df) # get slope of observed data

for (i in seq_len(n_sim)) {
  x_sim[i] <- get_metric(shuffle_data(df)) #run simulation
}

slopes <- list(x_obs,x_sim) 
get_pval(slopes)
plot_ran_test(slopes)

# Question 5 ------------------------------




. <- glm(family = 'binomial', df[,1]~df[,2])
. <- summary(.)
print(.)

# As we can see our p value with the real data is 0.161 but with the simulated data it is 0.059 for lower and 0.941 for upper. this does not really make sense because we would expect them to be closer or for the lower bound to be half the upper bound or some trend, but we do not see that. However, the code is done right so it might just be the difference between randomized data and a non-randomized sample.