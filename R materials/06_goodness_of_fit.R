
# Network load data from Exercise 8.2 

load <- read.table(file="load.txt", header=T)

> head(load)
  load
1 17.2
2 24.1
3 13.5
4 15.4
5 19.7
6 22.1
> 


# Histogram and boxplot

par(mfrow=c(1,2)) # 2 plots in 1 row

hist(load$load)
boxplot(load)

par(mfrow=c(1,1))

# Normal QQ plot

qqnorm(load$load)
qqline(load$load)

# Do these data come from a uniform distribution?

x <- load$load
y <- runif(100, min=min(x), max=max(x))

qqplot(x,y)
abline(a=0,b=1)

# Testing normality


# First download and install "nortest" packages using 
# install.packages("nortest")

# Load the package in R

library(nortest)

> shapiro.test(x)

	Shapiro-Wilk normality test

data:  x 
W = 0.9782, p-value = 0.4787

> 

> pearson.test (x)

	Pearson chi-square normality test

data:  x 
P = 2.4, p-value = 0.9344

> 

# On page 310, the book gives a p-value of 0.78. Investigate what 
# causes the difference by reading how pearson.test forms the bins

