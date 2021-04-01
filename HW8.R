# -----------------------------------
# Script for running an ANOVA on random data
# 31 Mar 2021
# EMD
# -----------------------------------
#

# import packages ------------------------------

library(ggplot2)

# create data ------------------------------

# Control survival when exposed to lc50 with survivors being 1

Survival_C = rnorm(1000, mean = 0.5, sd = 1)

# 0.25mg curcumin exposure survival rate

Survival_L = rnorm(1000, mean = 0.35, sd = 1)

# 1mg curcumin exposure survival rate

Survival_H = rnorm(1000, mean = 0.25, sd = 1)

# Organize data ------------------------------

survival_total = data.frame(ID = 1:3000, c(Survival_C, Survival_L, Survival_H), Treatment = c(rep('control', 1000), rep('low_curcumin', 1000), rep('high_curcumin', 1000)))

colnames(survival_total)[2] <- ('Survival')

# analyze the data ------------------------------

summary_surv = summary(survival_total)
print(summary_surv)

stats <- aov(Survival ~ Treatment, data = survival_total)

stats_sum <- summary(stats)

print(stats_sum)

# create a boxplot ------------------------------

ano_plot <- ggplot(stats) + aes(x = Treatment, y = Survival) + geom_boxplot()

print(ano_plot)

# Do it again ------------------------------

# create data ------------------------------

# Control survival when exposed to lc50 with survivors being 1

Survival_C = rnorm(1000, mean = 0.5, sd = 1)

# 0.25mg curcumin exposure survival rate

Survival_L = rnorm(1000, mean = 0.4, sd = 1)

# 1mg curcumin exposure survival rate

Survival_H = rnorm(1000, mean = 0.25, sd = 1)

# Organize data ------------------------------

survival_total = data.frame(ID = 1:3000, c(Survival_C, Survival_L, Survival_H), Treatment = c(rep('control', 1000), rep('low_curcumin', 1000), rep('high_curcumin', 1000)))

colnames(survival_total)[2] <- ('Survival')

# analyze the data ------------------------------

summary_surv = summary(survival_total)
print(summary_surv)

stats <- aov(Survival ~ Treatment, data = survival_total)

stats_sum <- summary(stats)

print(stats_sum)

# create a boxplot ------------------------------

ano_plot <- ggplot(stats) + aes(x = Treatment, y = Survival) + geom_boxplot()

print(ano_plot)

# Adjust the Means ------------------------------

# create data ------------------------------

# Control survival when exposed to lc50 with survivors being 1

Survival_C = rnorm(1000, mean = 0.5, sd = 1)

# 0.25mg curcumin exposure survival rate

Survival_L = rnorm(1000, mean = 0.46, sd = 1)

# 1mg curcumin exposure survival rate

Survival_H = rnorm(1000, mean = 0.43, sd = 1)

# Organize data ------------------------------

survival_total = data.frame(ID = 1:3000, c(Survival_C, Survival_L, Survival_H), Treatment = c(rep('control', 1000), rep('low_curcumin', 1000), rep('high_curcumin', 1000)))

colnames(survival_total)[2] <- ('Survival')

# analyze the data ------------------------------

summary_surv = summary(survival_total)
print(summary_surv)

stats <- aov(Survival ~ Treatment, data = survival_total)

stats_sum <- summary(stats)

print(stats_sum)

# create a boxplot ------------------------------

ano_plot <- ggplot(stats) + aes(x = Treatment, y = Survival) + geom_boxplot()

print(ano_plot)

# Adjust the Replicates ------------------------------

# create data ------------------------------

# Control survival when exposed to lc50 with survivors being 1

Survival_C = rnorm(360, mean = 0.5, sd = 1)

# 0.25mg curcumin exposure survival rate

Survival_L = rnorm(360, mean = 0.4, sd = 1)

# 1mg curcumin exposure survival rate

Survival_H = rnorm(360, mean = 0.3, sd = 1)

# Organize data ------------------------------

survival_total = data.frame(ID = 1:360, c(Survival_C, Survival_L, Survival_H), Treatment = c(rep('control', 360), rep('low_curcumin', 360), rep('high_curcumin', 360)))

colnames(survival_total)[2] <- ('Survival')

# analyze the data ------------------------------

summary_surv = summary(survival_total)
print(summary_surv)

stats <- aov(Survival ~ Treatment, data = survival_total)

stats_sum <- summary(stats)

print(stats_sum)

# create a boxplot ------------------------------

ano_plot <- ggplot(stats) + aes(x = Treatment, y = Survival) + geom_boxplot()

print(ano_plot)

