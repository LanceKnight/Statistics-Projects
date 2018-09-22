

# use install.packages("boot") to first install 
# the package and then load it

library(boot)

# read the cpu data (we have seen these before)

> (cpu <- scan(file="cputime.txt"))
Read 30 items
 [1]  70  36  43  69  82  48  34  62  35  15  59 139  46  37  42  30  55  56
[19]  36  82  38  89  54  25  35  24  22   9  56  19
> 

# Parameter of interest: Median

###########################
# Nonparametric Bootstrap #
###########################

median.npar <- function(x, indices) {
  result <- median(x[indices])
  return(result)
  }

> (median.npar.boot <- boot(cpu, median.npar, R=999, sim="ordinary", stype="i"))

ORDINARY NONPARAMETRIC BOOTSTRAP


Call:
boot(data = cpu, statistic = median.npar, R = 999, sim = "ordinary", 
    stype = "i")


Bootstrap Statistics :
    original    bias    std. error
t1*     42.5 0.6721722    5.876943
> 

# Let's verify the calculations

# See what's else is stored in median.npar.boot

> names(median.npar.boot)
 [1] "t0"        "t"         "R"         "data"      "seed"      "statistic"
 [7] "sim"       "call"      "stype"     "strata"    "weights"  
> 

> median(cpu)
[1] 42.5
>
> median.npar.boot$t0
[1] 42.5
> 
> mean(median.npar.boot$t)-median.npar.boot$t0
[1] 0.6721722
> 
> sd(median.npar.boot$t)
[1] 5.876943
Warning message:
sd(<matrix>) is deprecated.
 Use apply(*, 2, sd) instead. 
> 

# See the bootstrap distribution of median estimate

plot(median.npar.boot)

# Get the 95% confidence interval for median

> boot.ci(median.npar.boot)
BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
Based on 999 bootstrap replicates

CALL : 
boot.ci(boot.out = median.npar.boot)

Intervals : 
Level      Normal              Basic         
95%   (30.31, 53.35 )   (29.50, 49.50 )  

Level     Percentile            BCa          
95%   (35.5, 55.5 )   (35.0, 55.5 )  
Calculations and Intervals on Original Scale
Warning message:
In boot.ci(median.npar.boot) :
  bootstrap variances needed for studentized intervals
> 

# Let's verify 

# Normal approximation method

> c(42.5 - 0.6721722 - qnorm(0.975) * 5.876943, 
    42.5 - 0.6721722 - qnorm(0.025) * 5.876943)
[1] 30.30923 53.34642
> 

# Percentile bootstrap method

> sort(median.npar.boot$t)[c(25, 975)]
[1] 35.5 55.5
> 

# Basic bootstrap method

> c(2*42.5-55.5, 2*42.5-35.5)
[1] 29.5 49.5
> 


###########################
# Parametric Bootstrap #
###########################

# Saw earlier that a Gamma distribution fit well to these data

#######
# Need a function to get MLE of median

# First, a function for computing negative log-likelihood

neg.loglik.fun <- function(par, dat)
{	result <- sum(dgamma(dat, shape=par[1], rate=par[2], log=TRUE))
	return(-result)
	}

# Next, a function to get MLEs of parameters

par.mle <- function (dat, par.init=c(3, 0.01))
{
ml.est <- optim(par=par.init, fn=neg.loglik.fun, method = "L-BFGS-B", 
				lower=rep(0,2), dat=dat)$par
names(ml.est) <- c("shape", "rate")	
return(ml.est)	
	}

# Then, a function get MLE of median

median.mle <- function(dat) 
	{ 
# MLE of model parameters
par.est <- par.mle(dat)
# MLE of median
median.est <- qgamma(0.5, shape = par.est["shape"], rate = par.est["rate"])
return(median.est)
	}

###########

# Get MLE of model parameters 

> (ml.est <- par.mle(cpu))
     shape       rate 
3.63149628 0.07529459 
>  

# MLE of median

> (median.mle(cpu))
[1] 43.88304
> 

# Need a function that simulates parametric resamples

gamma.resamp <- function(dat, ml.est) { 
  xsim <- rgamma(length(dat), ml.est$shape, ml.est$rate)
  return(xsim)
  }

# Now use the boot function to generate resamples

> (median.par.boot <- boot(cpu, median.mle, R=99, sim="parametric", 
						ran.gen=gamma.resamp,
                         mle=list(shape=ml.est["shape"], rate=ml.est["rate"])))
PARAMETRIC BOOTSTRAP


Call:
boot(data = cpu, statistic = median.mle, R = 99, sim = "parametric", 
    ran.gen = gamma.resamp, mle = list(shape = ml.est["shape"], 
        rate = ml.est["rate"]))


Bootstrap Statistics :
    original      bias    std. error
t1* 43.88304 -0.05082498     4.28127
> 

# But boot.ci does not give the confidence intervals

> boot.ci(boot.ci(median.par.boot))
Error in empinf(boot.out, index = index, t = t.o, ...) : 
  influence values cannot be found from a parametric bootstrap
In addition: Warning message:
In boot.ci(median.par.boot) :
  bootstrap variances needed for studentized intervals
> 


