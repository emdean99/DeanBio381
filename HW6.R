# Homework 6
# March 10 2021
# EMD

#--------------------------------

## Part 1

# Assign random integer 3-10 to variable

n_dims <- as.integer(runif(1, 3, 10))
print(n_dims)

# Create a vector of variables from 1 to n_dims^2

my_vec <- 1:n_dims^2

# shuffle numbers
shuf_vec <- sample(my_vec)
print(shuf_vec)

# create a matrix
my_matrix <- matrix(shuf_vec, nrow=n_dims)

# print matrix
print(my_matrix)

# transpose the matrix
m_transpose <- t(my_matrix)

# print the transposed matrix
# transpose means the rows and columns are swittched

print(m_transpose)

# Find the sum and mean of the first and last rom

SumF <- sum(m_transpose[1,])

print(SumF)  
  
SumL <- sum(m_transpose[nrow(m_transpose),])

print(SumL)
  
MeanF <- mean(m_transpose[1,])

print(MeanF)
  
MeanL <- mean(m_transpose[nrow(m_transpose),])

print(MeanL)

# Read and use eigen()
val_eigen<- eigen(m_transpose, symmetric=TRUE)

# Find values and vectors and find what type they are
# The type of value they both are is double and the type of numbers are not integers

val <- val_eigen$values

typeof(val)

vec <- val_eigen$vectors

typeof(vec)

#---------------------------------

## Part 2

# Create the elements to assign to the list

my_matrix <- matrix(runif(16), nrow = 4)

my_logical <- .5 < runif(100)

my_letters <- sample(letters)

# Create the list

my_list <- list(my_matrix,my_logical,my_letters)
print(my_list)

# New list

new_list <- list(my_matrix[2,2],my_logical[2],my_letters[2])
print(new_list)

# Make new vector
new_vec <- c(my_matrix[2,2],my_logical[2],my_letters[2])

# Type of the new vector with underlying elements
typeof(new_vec)

#-------------------------

## Part 3

# Create the variables for the Data Frame

my_unis <- runif(26, 0, 10)
my_letters <- sample(letters)

# Combine to the dataframe
d_frame <- data.frame(my_unis,my_letters)
print(data.frame)

# Select 4 random rows and replace the numbers of my_unis with NA
d_frame$my_unis[as.integer(runif(4, 0, 26))] <- NA

# Identify which values are NA
is.na.data.frame(d_frame$my_unis)

# Sort in alphabetical order
new_df <- d_frame[order(d_frame[,'my_letters']), ]
print(new_df)

# process to calculate mean, first move the data to a variable
mean_data <- d_frame$my_unis
print(mean_data)
# remove NA
mean_data <- na.omit(mean_data)
# Show no NA
print(mean_data)
# Finally calculate mean
mean(mean_data)