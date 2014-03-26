# Bootstrapping the mean
theta <- 1
x <- runif(100,0,theta)
max.boot <- replicate(1e5,mean(sample(x,replace=TRUE)))
sd(max.boot)

# Compare with theoretical formula
sd(x)/sqrt(length(x))

# distribution of the mean
hist((max.boot-mean(x))/(sd(max.boot)),breaks="Scott",freq=FALSE,ylim=c(0,0.4))
xs <- seq(-4,4,length.out=200)
lines(xs,dnorm(xs,0,1),col="red",lwd=2)

##################
# Example 1      #
##################
lawstat <- read.table("lawstat.dat")
s <- c(4,6,13,15,31,35,36,45,47,50,52,53,70,79,82)

# Given data of 15 observations
d <- lawstat[s,]
cor(d$LSAT,d$GPA)

# Calculate std? Use bootstrap
B<-3200
cor.boot <- rep(0, B)
for (i in 1:B){
  ind <- round(15*runif(15,0,1))
  cor.boot[i] <- cor(d[ind,])[2]
}

sd(cor.boot)

# We have the full data = population
B<-3200
corr <- rep(0, B)
for (i in 1:B){
  ind <- round(82*runif(15,0,1))
  corr[i] <- cor(lawstat[ind,])[2]
}

sd(corr)

# Display histograms
layout(matrix(c(1,2),2,1))
hist(cor.boot)
hist(corr)

layout(matrix(c(1),1,1))
boxplot(cor.boot,corr)

# plot bootstrap ecdf and true one
plot(ecdf(cor.boot),verticals=TRUE,do.points=FALSE)
lines(ecdf(corr),verticals=TRUE,do.points=FALSE,col="red")

# textbook formula for sd(corr) only valid if the population is bivariate normal
(1-cor(d)[2]^2)/sqrt(15-3)

##############################
# Example 2: Regression      #
##############################
library(boot)
library(ISLR)
data(Auto)
boot.fn <- function (data,index){
  return (coef(lm(mpg~horsepower ,data=data ,subset =index)))
}

# use bootstrap
boot(Auto ,boot.fn ,1000)
# Compare
summary (lm(mpg~horsepower ,data=Auto))$coef

# second order model
boot.fn<-function (data ,index ){
  coefficients(lm(mpg~horsepower +I(horsepower^2),data=data,subset =index))
}
boot(Auto ,boot.fn ,1000)

# compare
summary (lm(mpg~horsepower+I(horsepower^2) ,data=Auto))$coef