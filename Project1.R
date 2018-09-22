#########################################################
#This part is for initialization for 1000 experiments
########################################################

rm(list=ls())#clear working space
expData1 = c()#an array for holding 1000 experiments results for days to get whole system repaired in each experiment
expData2 = rep( 0,times = 1000 )#an array for holding the case where all computers are infected
expData3 = c()#an array for the number of infected computers in each experiment
expNum = 1 #the index of the number of experiment

while( expNum <= 1000){# using Monte Carlo Method, do 1000 experiment to estimate

  ####################################################
  #This part is for initialization for each experiment
  ####################################################
  
  A = rep(0, times = 20)# create an array of 20 values of 0's meaning 20 uninfected computers
  A[1] = 1 # set the first value in A meaning a computer is infected
  B= rep( 0, times = 20 ) # create an array to record whether a certain computer has been infected
  sysVirusRmvd = F # a parameter to determine where the whole system is repaired
  days = 0 # the days it takes to get the whole system repaired

  while( sysVirusRmvd != T ){#if the virus still exist in any of the 20 computers, loop
    
    days = days + 1#increase the days
    repaired = 0 # the number of computers that the technician has fixed
    X = sum(A)# counting the number of current infected computers

  ###########################
  #This part is for infecting
  ###########################
  
  for( i in 1:20 ){#scan every computer
    
    if( A[i] == 0 ){# if a computer is uninfected
      
      rnd = runif(1, 0, 1)# generate a random number from uniform distribution
      
      if( rnd < 1 - 0.9 ^ X) A[i] = 1# get it infected if the random number is less than 1-0.9^ X, which is the infection rate for the number of current infected computers.
    }
    
  }
  ######################################################################################################
  #This part is for checking whether a certain computer has been infected, if yes, set the coresponding position in B to 1
  ######################################################################################################
  
  for ( j in 1: 20){#scan 20 computers
    
    if ( A[j] == 1 )#if a certain one is infected
      
      B[j] =1  # label it in B by setting the correspoding position to 1
  }
  
  X = sum(A)# counting the number of current infected computers
  
  ##########################
  #This part is for repairing
  ##########################
  
  k = 1 #loop parameter
  
  while( (repaired < 5) & (k < 20)){# if the repaired computer is less than 5 or we have not scanned 20 computers yet,  loop
  
      if( A[k] == 1 ) {# if it is infected
        
        A[k] = 0 # get it repaired by setting it back to 0
        repaired = repaired + 1 # increase the number of computer the technician repair for this day
      }
      
      k = k + 1 # increase the loop parameter
      
  }
  
  #########################
  #This part is for checking whether the whole system is repaired
  ########################
  
    for(i in 1:20){# scan all 20 computers
      
      if( A[i]== 1 ) # if there is still an infected computer
        
        break # break the loop
    }
    
     if( i == 20) # if the i reaches 20, meaning after examing 20 computer we do not find a infected computer
       sysVirusRmvd = T # set the sysVirusRmvd, meaning virus has been removed for the system

  }  

  expData1[ expNum ] = days # record the days it taks in the expData1 array
  
  if (sum(B) == 20) # if the sum(B) == 20, it means all computers has been infected at least once
    expData2[ expNum ] = 1 # mark this day "1" in the expData2, otherwise the value in expData2 stays 0, which was set when array was created
  
  expData3[ expNum ] = sum( B ) # count the number of infected computer
  expNum = expNum + 1 # increase the experiment index
}

###########################
#result printing part
###########################

print( "The average expected time is " )
print( mean( expData1 ) ) # get the mean of expData1 and it is the estimator of expected days it takes to get 20 computers fixed

print( "The probability that each computer gets infected at least once " )
print( sum( expData2 )/1000 ) # we get the probability from the number of experiment in which 20 computer has been infected at least once divided by 1000, the number of experiments

print( "The expected number of computers that get infected " )
print( mean( expData3 ) ) # get the mean of expData2 and it is the estimator os expected computers that get infected
