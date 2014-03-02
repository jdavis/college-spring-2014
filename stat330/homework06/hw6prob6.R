# This code will generate N=100000 values that are close to standard normal.
#
# Let U be a matrix of random values from Unif(0,1)

# Initialize random number generator
set.seed(2014)
N <- 100000
P <- 12

U = matrix(runif(n=N*P, min=0, max=1), N, P)
S = rowSums(U) - P/2

for (k in -3:3){
    print(sprintf('Proportion under %g: %0.5f.  P(Z < %g) = %0.5f',
       k, sum(S < k)/N, k, pnorm(k, mean=0, sd=1)))
}
