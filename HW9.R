# -----------------------------------
# A script that moves the information from HW8 into structured programming
# 07 Apr 2021
# EMD
# -----------------------------------
#

# Import libraries ------------------------------

library(ggplot2)


# -----------------------------------
# FUNCTION create_data
# description: this function creates data if there is no data input
# inputs: a null variable that will be then manipulated
# outputs: the output of this function should be three strings of data for separate trials
#####################################
create_data <- function(x=NULL) {
  if (is.null(x)) {
    Survival_C = rnorm(1000, mean = 0.5, sd = 1)
    Survival_L = rnorm(1000, mean = 0.35, sd = 1)
    Survival_H = rnorm(1000, mean = 0.25, sd = 1)
  }

  d8ta <- c(Survival_C, Survival_L, Survival_H)
return(d8ta)
  
} # end of create_data
# -----------------------------------





# -----------------------------------
# FUNCTION org_data
# description: This function orders the data that was created from the create data function
# inputs: The input will be one list of strings
# outputs: the output will be a data frame
#####################################
org_data <- function(A = c(Survival_C = runif(1000), Survical_L = runif(1000), Survival_H = runif(1000))) {
  
  Survival_C <- A[1:1000]
  Survival_L <- A[1001:2000]
  Survival_H <- A[2001:3000]
  
  survival_total = data.frame(ID = 1:3000, c(Survival_C, Survival_L, Survival_H), Treatment = c(rep('control', 1000), rep('low_curcumin', 1000), rep('high_curcumin', 1000)))
  
  colnames(survival_total)[2] <- ('Survival')


return(survival_total)

} # end of org_data
# -----------------------------------





# -----------------------------------
# FUNCTION data_analyze
# description: this take the data and gets the summery stats
# inputs: a dataframe df
# outputs: outputs summary stats
#####################################
data_analyze <- function(x) {

  summary_surv = summary(x)
  print(summary_surv)
  
  stats <- aov(Survival ~ Treatment, data = x)
  
  stats_sum <- summary(stats)
  
return(all_stats)

} # end of data_analyze
# -----------------------------------





# -----------------------------------
# FUNCTION box_plot
# description: creates a box plot of the analyzed data
# inputs: stats of an AOV of the data   
# outputs: a box plot
#####################################
box_plot <- function(orged_data) {
  
  ano_plot <- ggplot(orged_data) + aes(x = Treatment, y = Survival) + geom_boxplot()
  

return(ano_plot)

} # end of box_plot
# -----------------------------------





# -----------------------------------
# FUNCTION data_compile
# description: this function uses other functions to import data from another file, analyze it and then output a graph plot of the data
# inputs: data
# outputs: the outputs of this are summary stats as well as a boxplot of the data
#####################################
data_compile <- function(x=NULL) {
  if (is.null(x)) {
    x <- create_data()
  }
  data_organized <- org_data(x)
  stats <- data_analyze(data_organized)
  plotted<- box_plot(data_organized)
  
  print(stats)
  print(plotted)
return()

} # end of function_name
# -----------------------------------

data_compile()
