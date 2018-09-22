

t_test = function( data, x0 ){ # use two-sided t-test
  xbar = mean( data )
  v = var( data )
  n = length( data )
  t = ( xbar - x0 )/ sqrt( v/ n ) 
  return( t )
}
  
pvalue = function( tobs, n ){ #get p-value
  result = 2*( 1- pt( abs( tobs ), n-1 ) )
  return( result )
}


exp = function( n, x0 ){ #get a p- value from a singe experiment
  data = rnorm( n, 0, 1 )
  t = t_test( data, x0 )
  result = pvalue( t, n )
  return( result )
}

all_exp = function( nsim, n ){ #get p-value from each experiment
  result = replicate( nsim, exp( n, 0) ) 
  
  return( result )
}

obs_count = function( all_exp ){# get the observed count
  result = c( sum( all_exp < 0.1), sum( all_exp > 0.1 & all_exp < 0.2), sum( all_exp >0.2 & all_exp < 0.3), sum( all_exp> 0.3 & all_exp< 0.4), sum(all_exp > 0.4 & all_exp< 0.5), sum(all_exp>0.5 & all_exp<0.6), sum( all_exp > 0.6 & all_exp < 0.7), sum( all_exp >0.7 & all_exp < 0.8), sum( all_exp>0.8 & all_exp < 0.9), sum( all_exp > 0.9 & all_exp < 1))
  return( result )
}

exp_count = function( nsim ){ # get the expected count
  result = nsim* rep( 1/10, 10)
  return(result)
}

n = 100
##using p.value to see whether it is uniform(0,1)
t = all_exp( 1000, n)
obs = obs_count(t)
exp = exp_count( 1000)
pivet = sum( ( (obs-exp)^2 / exp ) )
p.value = 1- pchisq( pivet, df = 9)

##using histgram to show whether it is uniform(0,1)
hist( t )

##using qqplot to show whether it is uniform
y = runif( 100,0, 1)
qqplot( y, t )
abline(a=0, b= 1)
