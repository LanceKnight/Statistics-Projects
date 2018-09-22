#To create an array of 'n' Values
nvalues<-matrix(c(5,10,30,100), nrow = 1, ncol = 4, byrow = FALSE,
                dimnames = NULL)

#To create an array to store the coverage probability  Values
pValues<-matrix(0, nrow = 4, ncol = 10000, byrow = TRUE,
               dimnames = NULL)

#To run for different 'n' values
for(i in 1:4)
{
  n<-nvalues[i]
  
  #for running large number of times t.test
  for ( j in 1:10000)
  {
    x<-rnorm(n,0,1)    
    T <- t.test(x, mu=0, alternative="two.sided") 
    pValues[i,j] <- T$p.value 
  }  
} 

#histogram for different values of n
par(mfrow=c(1,2))
hist(pValues[1,], main = "Histogram of p values for n=5");
hist(pValues[2,], main = "Histogram of p values for n=10");

par(mfrow=c(1,2))
hist(pValues[3,], main = "Histogram of p values for n=30");
hist(pValues[4,], main = "Histogram of p values for n=100");

par(mfrow=c(1,2))

#to generate Q-Q plot for pvalues generated for each 'n'
for(i in 1:4)
{  
qqnorm(pValues[i,])
qqline(pValues[i,])
x <- pValues[i,]
y <- runif(10000,0,1)
qqplot(x,y)
abline(a=0,b=1)
}

#To run pearson test on p-values generated for each value of 'n'
library(nortest)

(result<-pearson.test(pValues[1,]))

(result2<-pearson.test(pValues[2,]))

(result3<-pearson.test(pValues[3,]))

(result4<-pearson.test(pValues[4,]))


