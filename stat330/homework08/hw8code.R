#################################
##   random Z value generation 
#################################
N = 10000    # number of draws
S = matrix(ceiling(runif(N*33, 0, 20)), N, 33)
Y = rowMeans(S) - 10.5
## Y contains the randomly generated value 
## (that have roughly a standard normal distribution)
## Note: any difference between this distribution and standard normal
##    is due to the central limit theorem only being "approximately" normal.
sum(Y < c) / N    # This gives the proportion of Y less than c (you must provide c)


#############################
##   for M/M/1 simulation
#############################

N <- 100000
lambda_a <- 2
lambda_s <- 8
set.seed(2014)
iat <- rexp(N, lambda_a)
st <- rexp(N, lambda_s)
mat <- matrix(0, 7, N)
mat[1,] <- cumsum(iat)
mat[2,] <- mat[1,]
mat[3,] <- st
mat[4,] <- mat[2,] + mat[3,]

rep = 1
while(any(id <- (mat[2,col2 <- 2:N] < mat[4,col4 <- 1:(N-1)]))){ 
	mat[2,col2[id]] <- mat[4,col4[id]]
	mat[4,col2[id]] <- mat[2,col2[id]] + mat[3,col2[id]]
	mat[5,col2[id]] <- mat[5,col2[id]] + 1
	cat("Finished step",rep <- rep+1,"\n")
}

mat[6,] <- mat[4,] - mat[1,]
mat[7,] <- mat[6,] - mat[3,]
samp <- sample(1:ncol(mat), N/4)   # sample 25% of random arrival times
hist(mat[5,samp],breaks = -1:max(mat[5,samp])+.5,xlab='System state', 
	main='Histogram of X = number of items in system',prob=T)
{
	cat('\n******************************\n M/M/1 system characteristics \n\n')
	cat('      r =',sum(mat[5,samp] > 0)/length(samp),'\n')
	cat('   pi_0 =',sum(mat[5,samp] == 0)/length(samp),'\n')
	cat('   pi_1 =',sum(mat[5,samp] == 1)/length(samp),'\n')
	cat('   pi_2 =',sum(mat[5,samp] == 2)/length(samp),'\n')
	cat('P(X > 2)=',sum(mat[5,samp] > 2)/length(samp),'\n\n')
	cat('   Distribution summaries of number of items in system at steady state:\n')
	cat('   E(X) =',mean(mat[5,samp]),'\n')
	cat(' Var(X) =',var(mat[5,samp]),'\n\n')
	cat('   Expected number of items in system at steady state:\n')
	cat('   E(X) =',mean(mat[5,samp]),'\n')
	cat('  E(Xs) =',sum(mat[5,samp] > 0)/length(samp),'\n')
	cat('  E(Xw) =',mean(mat[5,samp])-sum(mat[5,samp] > 0)/length(samp),'\n\n')
	cat('   Expected time an item spends in system at steady state:\n')
	cat('   E(R) =',mean(mat[6,samp]),'\n')
	cat('   E(S) =',mean(mat[3,samp]),'\n')
	cat('   E(W) =',mean(mat[7,samp]),'\n')
	cat('\n******************************\n')
}

###################################
##   for M/M/infinity simulation
###################################

N <- 100000
lambda_a <- 1
lambda_s <- 1
set.seed(2014)
iat <- rexp(N, lambda_a)
st <- rexp(N, lambda_s)
mat <- matrix(0, 7, N)
mat[1,] <- cumsum(iat)
mat[2,] <- mat[1,]
mat[3,] <- st
mat[4,] <- mat[2,] + mat[3,]

sep = 1
while(any(id <- (mat[2,col2 <- (1+sep):N] < mat[4,col4 <- 1:(N-sep)]))){ 
	mat[5,col2[id]] <- mat[5,col2[id]] + 1
	cat("Finished step",sep,"\n")
	sep <- sep + 1
}

mat[6,] <- mat[4,] - mat[1,]
mat[7,] <- mat[6,] - mat[3,]

samp <- sample(1:ncol(mat), N/4)   # sample 25% of random arrival times
hist(mat[5,samp],breaks = -1:max(mat[5,samp])+.5,xlab='System state', 
	main='Histogram of X = number of items in system\nRed dots are Poisson(r), see p.193',prob=T)
points(a <- 0:max(mat[5,samp]), dpois(a,r <- mean(st)/mean(iat)), col='black', pch=1, cex=1.1)
points(a <- 0:max(mat[5,samp]), dpois(a,r <- mean(st)/mean(iat)), col='red', pch=16)
{
	cat('\n*************************************\n M/M/infinity system characteristics \n\n')
	cat('     (r =',r,'[used for plots])\n\n')
	cat('   pi_0 =',sum(mat[5,samp] == 0)/length(samp),'\n')
	cat('   E(X) =',mean(mat[5,samp]),'\n')
	cat(' Var(X) =',var(mat[5,samp]),'\n')
	cat('   E(R) =',mean(mat[6,samp]),'\n')
	cat('\n*************************************\n')
}


