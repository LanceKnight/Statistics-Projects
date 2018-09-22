n = 1000 # number of experiments
alpha = 0.05 # given confidence level 95%, alpha = 0.05

generateData = function( n, par ){ # gives n data from bernoulli distribution. par = p
  result = rbinom( n, 1, par )
  return( result )
}

CI = function( data, alpha ){ # get the CI from the data and alpha
  n = length( data) # get n
  p = sum( data ) / n # the estimator of p
  var = p*( 1 - p )/ n # get variance of p
  s = sqrt( var ) # get the square root of variance, which is the standard error of sample data
  t = qt( 1 - ( alpha / 2 ), n-1 ) # get pivet t
  result = p + c( -1, 1 ) * t * s # get the upper and lower bound
  return( result )
}

coverage = function( n, num, par, alpha ){ #get the coverage probability
  record = rep( 0, n ) # use a vector to store result of each experiments
  
  for ( i in 1:n ){ # for each experiment
    data = generateData( num, par ) # generate data
    ci = CI( data, alpha) # get the CI
    if( par >= ci[1] && par <= ci[2]){ # check if it covers the true value
      record[ i ] = 1 # if covers, set the value in record to 1
    }
  }
  result = sum( record ) / n # get the portion of experiments that covers real p
  return ( result )
}

# the following part is to get coverage probability from different pair of n and p
coverage.n5.p0.05 = coverage( n, 5, 0.05, alpha ) # n = 5, p = 0.05
coverage.n5.p0.1 = coverage( n, 5, 0.1, alpha ) # n = 5, p = 0.1
coverage.n5.p0.25 = coverage( n, 5, 0.25, alpha ) # n = 5, p = 0.25 
coverage.n5.p0.5 =  coverage( n, 5, 0.5, alpha ) # n = 5, p = 0.5
coverage.n5.p0.9 = coverage( n, 5, 0.9, alpha ) # n = 5, p = 0.9
coverage.n5.p0.95 = coverage( n, 5, 0.95, alpha ) # n = 5, p = 0.95

coverage.n10.p0.05 = coverage( n, 10, 0.05, alpha ) # n = 10, p = 0.05
coverage.n10.p0.1 = coverage( n, 10 , 0.1, alpha )# n= 10, p = 0.1
coverage.n10.p0.25 = coverage( n, 10, 0.25, alpha )#n = 10, p = 0.25
coverage.n10.p0.5 = coverage( n, 10, 0.5, alpha ) #n = 10, p = 0.5 
coverage.n10.p0.9 = coverage( n, 10, 0.9, alpha ) # n = 10, p = 0.9
coverage.n10.p0.95 = coverage( n, 10, 0.95, alpha ) # n = 10, p = 0.95  

coverage.n30.p0.05 =  coverage( n, 30, 0.05, alpha ) # n = 30, p = 0.05
coverage.n30.p0.1 = coverage( n, 30, 0.1, alpha ) # n = 30, p = 0.1
coverage.n30.p0.25 = coverage( n, 30, 0.25, alpha ) # n = 30, p = 0.25
coverage.n30.p0.5 = coverage( n, 30, 0.5, alpha ) # n = 30, p = 0.5
coverage.n30.p0.9 = coverage( n, 30, 0.9, alpha ) # n = 30, p = 0.9
coverage.n30.p0.95 = coverage( n, 30, 0.95, alpha ) # n = 30, p = 0.95

coverage.n100.p0.05 = coverage( n, 100, 0.05, alpha ) # n = 100, p = 0.05
coverage.n100.p0.1 = coverage( n, 100, 0.1, alpha ) # n = 100, p = 0.1 
coverage.n100.p0.25 = coverage( n, 100, 0.25, alpha ) # n = 100, p = 0.25
coverage.n100.p0.5 = coverage( n, 100, 0.5, alpha ) # n = 100, p = 0.5
coverage.n100.p0.9 = coverage( n, 100, 0.9, alpha ) # n = 100, p = 0.9
coverage.n100.p0.95 = coverage( n, 100, 0.95, alpha ) # n = 100, p = 0.95 


# get the coverage probability for each n, which will server as y value in the graph
coverage.n5 = c( coverage.n5.p0.05, coverage.n5.p0.1, coverage.n5.p0.25, coverage.n5.p0.5, coverage.n5.p0.9, coverage.n5.p0.95 )
coverage.n10 = c( coverage.n10.p0.05, coverage.n10.p0.1, coverage.n10.p0.25, coverage.n10.p0.5, coverage.n10.p0.9, coverage.n10.p0.95 ) 
coverage.n30 = c( coverage.n30.p0.05, coverage.n30.p0.1, coverage.n30.p0.25, coverage.n30.p0.5, coverage.n30.p0.9, coverage.n30.p0.95 )
coverage.n100 = c( coverage.n100.p0.05, coverage.n100.p0.1, coverage.n100.p0.25, coverage.n100.p0.5, coverage.n100.p0.9, coverage.n100.p0.95 )

# different p value serves as x value in the grahp
p = c(0.05,0.1,0.25,0.5,0.9,0.95 )

par(mfrow=c(1,1),mar=c(5,4,1,0))# one graph
plot( x = p, y = coverage.n5, type = "b", xlab = "p",ylab = "coverage",xlim = c(0,1), ylim = c(0,1),lty = 3)# draw n = 5 curve
lines( x = p, y = coverage.n10, type = "b",lty = 4) # add n = 10 curve
lines( x = p, y = coverage.n30, type = "b",lty = 5) # add n = 30 curve
lines( x = p, y = coverage.n100, type = "b",lty = 1) # add n = 100 curve
abline( h = 0.95,lty = 2 ) # add 0.95 horizon line

