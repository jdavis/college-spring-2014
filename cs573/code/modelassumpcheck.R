# Checking model assumptions
###############
### Example 1 #
###############
x <- seq(-3,3,length.out=200)
y <- 2+3*x+rnorm(200,0,1)
T1 <- lm(y~x)

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(T1)

# ACF plot
layout(matrix(c(1),1,1))
acf(resid(T1),type=c("correlation"))

###################################
### Example 2 (correlated errros) #
###################################
rho <- 0.5
e <- rnorm(1,0,2)
for (i in 2:200){
  e[i] <- rho*e[i-1]+rnorm(1,0,2)
}
# Center errors
e <- e-mean(e)
y <- 2+3*x+e
# Plot data
Tcor <- lm(y~x)
plot(y ~ x, pch=c("o"), col=c("red"))
abline(Tcor, lwd=2)
# Plot ACF
acf(resid(Tcor),type=c("correlation"))

# Hypothesis test
library(lawstat)
runs.test(resid(Tcor))

######################################
### Example 3 (nonconstant variance) #
######################################
y <- 2+3*x+rnorm(200,0,sapply(x,function(x){x^2+x+1}))
Tncvar <- lm(y~x)
plot(y ~ x, pch=c("o"), col=c("red"))
abline(Tncvar, lwd=2)

# Breusch-Pagan test
library(lmtest)
bptest(Tncvar)

