# Homework 7
# March 17 2021
# EMD

#--------------------------------

#Open Libraries
library(ggplot2) # for graphics
library(MASS) # for maximum likelihood estimation

# Copied to understand the workflow

# quick and dirty, a truncated normal distribution to work on the solution set

z <- rnorm(n=3000,mean=0.2)
z <- data.frame(1:3000,z)
names(z) <- list("ID","myVar")
z <- z[z$myVar>0,]
str(z)
summary(z$myVar)

# Plot histogram of data
p1 <- ggplot(data=z, aes(x=myVar, y=..density..)) +
  geom_histogram(color="grey60",fill="cornsilk",size=0.2) 
print(p1)

# Add Empirical Density Curve
p1 <-  p1 +  geom_density(linetype="dotted",size=0.75)
print(p1)

# Get maximum likelihood parameters
normPars <- fitdistr(z$myVar,"normal")
print(normPars)
str(normPars)
normPars$estimate["mean"] # note structure of getting a named attribute

# plot normal probability density
meanML <- normPars$estimate["mean"]
sdML <- normPars$estimate["sd"]

xval <- seq(0,max(z$myVar),len=length(z$myVar))

stat <- stat_function(aes(x = xval, y = ..y..), fun = dnorm, colour="red", n = length(z$myVar), args = list(mean = meanML, sd = sdML))
p1 + stat

# Plot exponential probability density
expoPars <- fitdistr(z$myVar,"exponential")
rateML <- expoPars$estimate["rate"]

stat2 <- stat_function(aes(x = xval, y = ..y..), fun = dexp, colour="blue", n = length(z$myVar), args = list(rate=rateML))
p1 + stat + stat2

# Plot Uniform probability density
stat3 <- stat_function(aes(x = xval, y = ..y..), fun = dunif, colour="darkgreen", n = length(z$myVar), args = list(min=min(z$myVar), max=max(z$myVar)))
p1 + stat + stat2 + stat3

# Plot gamma probability distribution

gammaPars <- fitdistr(z$myVar,"gamma")
shapeML <- gammaPars$estimate["shape"]
rateML <- gammaPars$estimate["rate"]

stat4 <- stat_function(aes(x = xval, y = ..y..), fun = dgamma, colour="brown", n = length(z$myVar), args = list(shape=shapeML, rate=rateML))
p1 + stat + stat2 + stat3 + stat4

# Plot beta probability density

pSpecial <- ggplot(data=z, aes(x=myVar/(max(myVar + 0.1)), y=..density..)) +
  geom_histogram(color="grey60",fill="cornsilk",size=0.2) + 
  xlim(c(0,1)) +
  geom_density(size=0.75,linetype="dotted")

betaPars <- fitdistr(x=z$myVar/max(z$myVar + 0.1),start=list(shape1=1,shape2=2),"beta")
shape1ML <- betaPars$estimate["shape1"]
shape2ML <- betaPars$estimate["shape2"]

statSpecial <- stat_function(aes(x = xval, y = ..y..), fun = dbeta, colour="orchid", n = length(z$myVar), args = list(shape1=shape1ML,shape2=shape2ML))
pSpecial + statSpecial

#--------------------------------------------------------

# Done with imported data

# Import
z <- read.table("dates_cacajao_aldina_measurements_and_hand_c_ouakary.csv",header=TRUE,sep=",", stringsAsFactors=FALSE)
str(z)
summary(z)

# Pull out the important data for analysis
z <- z$total_wt
z <- na.omit(z)

# Make into a data frame for analysis
z <- data.frame(1:116,z)
# Plot histogram of data
p1 <- ggplot(data=z, aes(x=z, y=..density..)) +
  geom_histogram(color="grey60",fill="cornsilk",size=0.2) 
print(p1)

# Add Empirical Density Curve
p1 <-  p1 +  geom_density(linetype="dotted",size=0.75)
print(p1)


# Get maximum likelihood parameters
normPars <- fitdistr(z$z,"normal")
print(normPars)
str(normPars)
normPars$estimate["mean"] # note structure of getting a named attribute

# plot normal probability density
meanML <- normPars$estimate["mean"]
sdML <- normPars$estimate["sd"]

xval <- seq(0,max(z$z),len=length(z$z))

stat <- stat_function(aes(x = xval, y = ..y..), fun = dnorm, colour="red", n = length(z$z), args = list(mean = meanML, sd = sdML))
p1 + stat

# Plot exponential probability density
expoPars <- fitdistr(z$z,"exponential")
rateML <- expoPars$estimate["rate"]

stat2 <- stat_function(aes(x = xval, y = ..y..), fun = dexp, colour="blue", n = length(z$z), args = list(rate=rateML))
p1 + stat + stat2

# Plot Uniform probability density
stat3 <- stat_function(aes(x = xval, y = ..y..), fun = dunif, colour="darkgreen", n = length(z$z), args = list(min=min(z$z), max=max(z$z)))
p1 + stat + stat2 + stat3

# Plot gamma probability distribution

gammaPars <- fitdistr(z$z,"gamma")
shapeML <- gammaPars$estimate["shape"]
rateML <- gammaPars$estimate["rate"]

stat4 <- stat_function(aes(x = z, y = ..y..), fun = dgamma, colour="brown", n = length(z$z), args = list(shape=shapeML, rate=rateML))
p1 + stat + stat2 + stat3 + stat4

# Plot beta probability density

pSpecial <- ggplot(data=z, aes(x=z/(max(z + 0.1)), y=..density..)) +
  geom_histogram(color="grey60",fill="cornsilk",size=0.2) + 
  xlim(c(0,1)) +
  geom_density(size=0.75,linetype="dotted")

betaPars <- fitdistr(x=z$z/max(z$z + 0.1),start=list(shape1=1,shape2=2),"beta")
shape1ML <- betaPars$estimate["shape1"]
shape2ML <- betaPars$estimate["shape2"]

statSpecial <- stat_function(aes(x = xval, y = ..y..), fun = dbeta, colour="orchid", n = length(z$z), args = list(shape1=shape1ML,shape2=shape2ML))
pSpecial + statSpecial
