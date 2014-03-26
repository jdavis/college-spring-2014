# Let X1,...,Xn be a i.i.d. samples from an unknown distribution F with mean mu and variance
# sigma^2 < infty. Suppose we are interested in the absolute value of the mean |mu|. 
# Let theta = |mu| and define hat{theta}=|xbar|.
# Then, Bootstrap estimates of the variance and centered distribution of hat{theta} are not consistent
# if mu = 0. 
library(boot)
set.seed(1)
n <- 100
mu<-0
x <- rnorm(n)+mu
bootfun <- function(x,i)
{ d <- x[i]
  abs(mean(d))
}
B <- 100000
bootres <- boot(x,bootfun,R=B)
hist(b<-sqrt(n)*(bootres$t-bootres$t0),br='FD',prob=T,
     ylim=c(0,1.1*2*dnorm(0)),xlab='',main='Failure of the bootstrap')

# add density of T
curve(dnorm(x)+dnorm(x+2*abs(mu)*sqrt(n)),-abs(mu)*sqrt(n),10,add=T,col='red',lwd=2)
curve(0*x,-10,-abs(mu)*sqrt(n),add=T,col='red',lwd=2)
#add density of T*
z1<-sqrt(n)*bootres$t0
curve(dnorm(x)+dnorm(x+2*abs(z1)),-abs(z1),10,add=T,col='blue',lwd=2)
curve(x*0,-10,-abs(z1),add=T,col='blue',lwd=2)
legend('topright',c('limit distr T*','limit distr T'),
       lwd=rep(2,2),lty=rep(1,2),col=c('blue','red'))
#Compare mean and variances of the bootstrap vs. truth of $|\bar X_n|$
mean(bootres$t)/(abs(mu)+2/sqrt(2*pi*n)*exp(-mu^2/2))
n*var(bootres$t)/(2*abs(mu)/sqrt(2*pi)*exp(-mu^2/2)-2/pi*exp(-mu^2)+2-pnorm(mu)-pnorm(-mu))