# -----------------------------------
# HW12
# 28 Apr 2021
# EMD
# -----------------------------------
#

# Preliminaries ------------------------------
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(patchwork)
library(TeachingDemos)

d <- PlantGrowth

d1 <- ToothGrowth
len <- d1$len
supp <- d1$supp
d2 <- data.frame(len, supp)
# Graph One ------------------------------

# First try making a basic graph with default options

b_graph1 <- ggplot(data=d, 
                 mapping=aes(x=group,
                             y=weight)) + 
  geom_boxplot()

print(graph1)                 

# Now try to assign some colors to the graph
b_graph2 <- ggplot(data=d, 
                 mapping=aes(x=group,
                             y=weight,
                             fill=group)) + 
  geom_boxplot()

print(b_graph2)

# Now change the background theme

theme_graph <- b_graph2 + theme_economist()
print(theme_graph)

# Now change the sizing so the legends stick out more

theme_graph <- b_graph2 + theme_economist(base_size = 10)
print(theme_graph)

# add some labels
b_graph3 <- ggplot(data=d, 
                   mapping=aes(x=group,
                               y=weight,
                               fill=group)) + 
  geom_boxplot() + theme_economist(base_size=10) + labs(title = "Plant Size Vs. Treatment Group", x = "Weight", y = "Treatment Group")
print(b_graph3)

# add all the plot data
b_graph4 <- ggplot(data=d, 
                   mapping=aes(x=group,
                               y=weight,
                               fill=group)) + 
  geom_boxplot() + theme_economist(base_size=10) + labs(title = "Plant Size Vs. Treatment Group", x = "Weight", y = "Treatment Group") + geom_point()
print(b_graph4)


# Make a second graph ------------------------------

# make a basic bar graph

t_graph1 <- ggplot(data=d2,
                   mapping=aes(x = supp, y = len)) +
  geom_col(data = d2)
print(t_graph1)

# change the colors
t_graph1 <- ggplot(data=d2,
                   mapping=aes(x = supp, y = len, fill = supp)) +
  geom_col(data = d2)
print(t_graph1)

# change the theme
theme_graph2 <- t_graph1 + theme_economist(base_size = 10)
print(theme_graph2)

# add some labels

t_graph2 <- ggplot(data=d2,
                   mapping=aes(x = supp, y = len, fill = supp)) +
  geom_col(data = d2) + theme_economist(base_size=10) + labs(title = "Cumulative Teeth size", x = "Length", y = "Treatment Group")
print(t_graph2)

# Combine the graphs to output together ------------------------------

final_graph <- b_graph4 + t_graph2
print(final_graph)
