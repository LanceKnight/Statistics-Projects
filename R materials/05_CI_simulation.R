
# A function to simulate data from a N(mu, sigma^2) distribution and computing CI 

conf.int <- function(mu, sigma, n, alpha){
	x <- rnorm(n, mu, sigma)
	ci <- mean(x) + c(-1,1) * qnorm(1-(alpha/2)) * sigma/sqrt(n)
	return(ci)
	}
	
# Get one CI

mu <- 5
sigma <- sqrt(10)
n <- 20
alpha <- 0.05

# > conf.int(mu, sigma, n, alpha)
# [1] 3.520961 6.292768
# > 

# Repeat the process nsim times


nsim <- 10000
ci.mat <- replicate(nsim, conf.int(mu, sigma, n, alpha))

# > dim(ci.mat)
# [1]     2 10000
# > 

# The first 5 intervals

# > ci.mat[, 1:5]
         # [,1]     [,2]     [,3]     [,4]     [,5]
# [1,] 3.689654 3.519999 3.466402 3.937424 3.140117
# [2,] 6.461462 6.291807 6.238210 6.709231 5.911925
# > 


# Graphing the first 100 intervals

plot(1:100, ci.mat[1, 1:100], ylim=c(min(ci.mat[,1:100]), max(ci.mat[,1:100])), 
		xlab="sample #", ylab="95% CI", type="p")
points(1:100, ci.mat[2, 1:100])
for (i in 1:100) {
	segments(i, ci.mat[1, i], i, ci.mat[2,i], lty=1)
	}
abline(h=5, lty=2)


# Proportion of times the interval is correct

# > mean( (mu >= ci.mat[1,])*(mu <= ci.mat[2,]) )
# [1] 0.9502
# > 
