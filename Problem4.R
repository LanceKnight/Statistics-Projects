library(boot)

median = qgamma(0.5,shape = 3, rate = 5)

ci.boot = function(dataNum, r){
  data = rgamma(dataNum, shape = 3, rate = 5)
  
  median.npar <- function(x, indices){
    result <- median(x[indices])
    return(result)
    
  }
  
  median.npar.boot <- boot(data, median.npar, R = r, sim = "ordinary",stype="i")
  
  result = boot.ci(median.npar.boot)
  return(result)
}




# exp = function( nsim ){
#   result = ci.boot(2000)
#   
# }


normal.coverage = function( n, median, dataNum, r){ #get the coverage probability
  record = rep( 0, n ) # use a vector to store result of each experiments
  
  
  for ( i in 1:n ){ # for each experiment
    ci = ci.boot(dataNum, r)$normal[1,2:3]
    if( median >= ci[1] && median <= ci[2]){ # check if it covers the true value
      record[ i ] = 1 # if covers, set the value in record to 1
    }
  }
  result = sum( record ) / n # get the portion of experiments that covers real p
  
  return ( result )
}


basic.coverage = function( n, median, dataNum, r){ #get the coverage probability
  record = rep( 0, n ) # use a vector to store result of each experiments
  
  
  for ( i in 1:n ){ # for each experiment
    ci = ci.boot(dataNum, r)$basic[1,4:5]
    if( median >= ci[1] && median <= ci[2]){ # check if it covers the true value
      record[ i ] = 1 # if covers, set the value in record to 1
    }
  }
  result = sum( record ) / n # get the portion of experiments that covers real p
  
  return ( result )
}



percent.coverage = function( n, median, dataNum, r){ #get the coverage probability
  record = rep( 0, n ) # use a vector to store result of each experiments
  
  
  for ( i in 1:n ){ # for each experiment
    ci = ci.boot(dataNum, r)$percent[1,4:5]
    if( median >= ci[1] && median <= ci[2]){ # check if it covers the true value
      record[ i ] = 1 # if covers, set the value in record to 1
    }
  }
  result = sum( record ) / n # get the portion of experiments that covers real p
  
  return ( result )
}


normal.coverage(1000, median, 5, 2000)
normal.coverage(1000, median, 10, 2000)
normal.coverage(1000, median, 30, 2000)
normal.coverage(1000, median, 100, 2000)

basic.coverage(1000, median, 5, 2000)
basic.coverage(1000, median, 10, 2000)
basic.coverage(1000, median, 30, 2000)
basic.coverage(1000, median, 100, 2000)

percent.coverage(1000, median, 5, 2000)
percent.coverage(1000, median, 10, 2000)
percent.coverage(1000, median, 30, 2000)
percent.coverage(1000, median, 100, 2000)

