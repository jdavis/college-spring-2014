# This code will generate a 95% confidence interval for an estimate
#    of the proportion of X >= Y to 3 decimals precision
#    by comparing 20 million random draws for X and Y

N <- 100000   # 100000 random draws for X and Y
px <- .4    # probability of success for r.v. X (MUST CHANGE THIS)
py <- .3    # probability of success for r.v. Y (MUST CHANGE THIS)

# we will run this simulation K=100 times to get K estimates for the proportion of X >= Y

K <- 200    # repeat the draws 200 times
save.prop <- rep(NA, length=K)
for (k in 1:K){
	# Initialize random number generator
	set.seed(k)
	
	X <- rgeom(N, prob=px) + 1
	Y <- rgeom(N, prob=py) + 1
	# Add 1 to these random values because in R the geometric random variable
	#    represents the number of failures until the first success,
	#    whereas in Baron this represents the number of trials until first success.
	# Geometric R.V. (in R) + 1 = Geometric R.V. (in Baron/class)
	
	print(sprintf(' Run %i.  Proportion of X >= Y: %0.5f,  for simulation size N = %i', 
	   k, save.prop[k] <- sum(X >= Y)/N, N))
}

# analyze the estimate of proportion X >= Y
est.mean <- mean(save.prop)
est.sd <- sd(save.prop)

# Determine a 95% confidence interval for the estimate (We'll learn this in Chapter 9.)
est.ci <- qnorm(c(.025,.975), mean=est.mean, sd=est.sd/sqrt(K))
	
print(sprintf('The final estimate for Proportion X >= Y is %0.5f.', est.mean))
print(sprintf('We are 95 percent confident that the true proportion of X >= Y is between %0.5f and %0.5f', 
	est.ci[1], est.ci[2]))

# If there is no difference between the endpoints for the 95% confidence interval
#    to 3 decimals, then we are 95% confident we have determined the estimate to 3 decimals:	
if (diff(round(est.ci,3))){
	print('We have not determined the true proportion of X >= Y to 3 decimals. Try increasing N or K.')
} else {
	print(sprintf('We have determined that the true proportion of X >= Y to 3 decimals is %0.3f.', mean(est.ci)))
}
	