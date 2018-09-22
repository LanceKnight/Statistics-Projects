
n = 1000 # number of experiments

generateData = function( num, par){ # generate Data
  result = runif( num, max = par)
  return( result )
}

MLE = function( data ){ # generate MLE, which is the max of data
  result = max( data ) 
  return( result )
}

MOME = function( data ){ # generate MOME, which is the 2 * mean( data )
  result = 2 * mean( data )
  return( result )
}


MSE = function( estPar, realPar ){ # calculate the mean squared error
  result = mean( ( estPar - realPar )^2 )
  return(result)
}

meanMSE.MLE = function( n, num, par ){ # get the mean of MSE for 1000 experiments of MLE
  mse = rep( 0, times = n ) # a vector to record result of experiments
  for( i in 1:n ){
    data = generateData( num, par ) # generate data
    mle = MLE( data )# get mle
    mse[i] = MSE( mle, par) # get mse for mle
  }
  result = mean( mse )
  return (result)
}

meanMSE.MOME = function( n, num, par ){ # get the mean of MSE for 1000 experiments of MOME
  mse = rep( 0, times = n ) # a vector to record result of experiments
  for( i in 1:n ){
    data = generateData( num, par ) # generate data
    mome = MOME( data ) # get mome
    mse[i] = MSE( mome, par) # get mse for mome
  }
  result = mean( mse )
  return (result)
}

#n = 5
MSE.MLE.n5.theta1 = meanMSE.MLE( n, 5, 1 ) # theta = 1, MLE
MSE.MOME.n5.theta1 = meanMSE.MOME( n, 5, 1) # theta = 1, MOME

MSE.MLE.n5.theta2 = meanMSE.MLE( n, 5, 2 ) # theta = 2, MLE
MSE.MOME.n5.theta2 = meanMSE.MOME( n, 5, 2) # theta = 2, MOME

MSE.MLE.n5.theta4 = meanMSE.MLE( n, 5, 4 ) # theta = 4, MLE
MSE.MOME.n5.theta4 = meanMSE.MOME( n, 5, 4) # theta = 4, MOME

#n = 10 
MSE.MLE.n10.theta1 = meanMSE.MLE( n, 10, 1 ) # theta = 1, MLE
MSE.MOME.n10.theta1 = meanMSE.MOME( n, 10, 1) # theta = 1, MOME

MSE.MLE.n10.theta2 = meanMSE.MLE( n, 10, 2 )# theta = 2, MLE
MSE.MOME.n10.theta2 = meanMSE.MOME( n, 10, 2)# theta = 2, MOME

MSE.MLE.n10.theta4 = meanMSE.MLE( n, 10, 4 ) # theta = 4, MLE
MSE.MOME.n10.theta4 = meanMSE.MOME( n, 10, 4) # theta = 4, MOME

#n = 30
MSE.MLE.n30.theta1 = meanMSE.MLE( n, 30, 1 ) # theta = 1, MLE
MSE.MOME.n30.theta1 = meanMSE.MOME( n, 30, 1) # theta = 1, MOME

MSE.MLE.n30.theta2 = meanMSE.MLE( n, 30, 2 ) # theta = 2, MLE
MSE.MOME.n30.theta2 = meanMSE.MOME( n, 30, 2) # theta = 2, MOME

MSE.MLE.n30.theta4 = meanMSE.MLE( n, 30, 4 ) # theta = 4, MLE
MSE.MOME.n30.theta4 = meanMSE.MOME( n, 30, 4) # theta = 4, MOME

#n = 100
MSE.MLE.n100.theta1 = meanMSE.MLE( n, 100, 1 ) # theta = 1, MLE
MSE.MOME.n100.theta1 = meanMSE.MOME( n, 100, 1) # theta = 1, MOME

MSE.MLE.n100.theta2 = meanMSE.MLE( n, 100, 2 ) # theta = 2, MLE
MSE.MOME.n100.theta2 = meanMSE.MOME( n, 100, 2)# theta = 2,MOME

MSE.MLE.n100.theta4 = meanMSE.MLE( n, 100, 4 ) # theta = 4, MLE
MSE.MOME.n100.theta4 = meanMSE.MOME( n, 100, 4) # theta = 4, MOME

theta = c( 1, 2, 4) # x axis
MSE.MLE.n5 = c( MSE.MLE.n5.theta1, MSE.MLE.n5.theta2, MSE.MLE.n5.theta4)# y axis for n= 5, MLE
MSE.MOME.n5 = c( MSE.MOME.n5.theta1, MSE.MOME.n5.theta2, MSE.MOME.n5.theta4 )# y axis for n = 5, MOME
MSE.MLE.n10 = c( MSE.MLE.n10.theta1, MSE.MLE.n10.theta2, MSE.MLE.n10.theta4)# y axis for n= 10, MLE
MSE.MOME.n10 = c( MSE.MOME.n10.theta1, MSE.MOME.n10.theta2, MSE.MOME.n10.theta4 )# y axis for n = 10, MOME

MSE.MLE.n30 = c( MSE.MLE.n30.theta1, MSE.MLE.n30.theta2, MSE.MLE.n30.theta4) # y axis for n = 30, MLE
MSE.MOME.n30 = c( MSE.MOME.n30.theta1, MSE.MOME.n30.theta2, MSE.MOME.n30.theta4 )#y axis for n = 30, MOME

MSE.MLE.n100 = c( MSE.MLE.n100.theta1, MSE.MLE.n100.theta2, MSE.MLE.n100.theta4)# y axis for n = 100, MLE
MSE.MOME.n100 = c( MSE.MOME.n100.theta1, MSE.MOME.n100.theta2, MSE.MOME.n100.theta4 )# y axis for n = 100, MOME

par(mfrow=c(2,2),mar=c(5,4,1,0))# 4 graph in 2 rows, 2 colums
plot( x = theta, y = MSE.MLE.n5,sub = "n=5",type = "b", xlab = "theta",ylab = "MSE",xlim = c(1,4), ylim = c(0,1.2))
lines( x = theta, y = MSE.MOME.n5, type = "b",lty = 2)


plot( x = theta, y = MSE.MLE.n10,sub = "n=10", type = "b", xlab = "theta",ylab = "MSE",xlim = c(1,4), ylim = c(0,1.2))
lines( x = theta, y = MSE.MOME.n10, type = "b",lty = 2)


plot( x = theta, y = MSE.MLE.n30,sub = "n=30", type = "b", xlab = "theta",ylab = "MSE",xlim = c(1,4), ylim = c(0,0.5))
lines( x = theta, y = MSE.MOME.n30, type = "b",lty = 2)


plot( x = theta, y = MSE.MLE.n100,sub = "n=100",type = "b", xlab = "theta",ylab = "MSE",xlim = c(1,4), ylim = c(0,0.5))
lines( x = theta, y = MSE.MOME.n100, type = "b",lty = 2)

