# Homework 5 assignment
# March 3 2021
# EMD

# Question 1

# assign x
 x <- 1.1
 
# assign y
 
 a <- 2.2
 
# assign b
 
 b <- 3.3
 
# Function a

z <- (x^(a^b)) 

print(z)

# Function b

z <- (x^a)^b

print(z)

# Function c

z <- 3*x^3 + 2*x^2 + 1

print(z)

# Question 2

# Vector a

z = c(1:8,7:1)

print(z)

# Vector b

z = 1:5

vec <- rep(z,z)

print(vec)

# Vector 3

x = 1:5
z = 5:1

vec <- rep(z,x)

print(vec)

# Question 3

# create variables
runi1 <- runif(2)
  
# get the angle

at <- atan(runi1[2]/runi1[1])

# Get the radius

r <- runi1[2] / sin(at)

# create the polar quardinates

polq <- c(r,at)

# Display the polar quardinates

print(polq)

# Question 4

# Set initial conditions

queue <- c("sheep", "fox", "owl", "ant")

# Serpent gets in line

queue <- c(queue, 'serpent')

# confirm update

print(queue)

# The sheep enters

queue <- queue[-1]

# confirm update

print(queue)

# The donkey arrives

queue <- c('donkey',queue)

# confirm update

print(queue)

# Serpent leaves

queue <- queue[-5]

# confirm update

print(queue)

# Owl leaves

queue <- queue[-3]

# confirm update

print(queue)

# The aphid arrives and the ant invides him to cut in line

queue <- c(queue[1],queue[2],'aphid',queue[3])

# confirm update

print(queue)

# Determine the position

queue == 'aphid'

# Question 5

# Create sequence

num = 1:100

# Find which are divisable by 2

num2 = num%%2 > 0
print(num)
print(num2)

# Assign values divisible by 2 to num
num <- num[num2]
print(num)

# Find which are divisible by 3
num3 = num%%3 > 0
print(num)
print(num3)

# Assign values divisible by 3 to num
num <- num[num3]
print(num)

#Find which are divisible by 7
num7 = num%%7 > 0
print(num)
print(num7)

# Assign values divisible by 7 to num
num <- num[num7]
print(num)

