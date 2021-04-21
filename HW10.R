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


# Question 3 ------------------------------

# -----------------------------------
# FUNCTION matrix
# description: this function creates a matrix with two column and row numbers specified
# inputs: c and r
# outputs: outputs a matrix
#####################################
matrix <- function(c = 4, r = 5) {
  
  mat <- matrix(runif(20), nrow = 4)
  

return(mat)

} # end of matrix
# -----------------------------------


