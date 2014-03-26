theta <- 1
x <- runif(100,0,theta)

# NONPARAMETRIC BOOTSTRAP
max.boot <- replicate(1e4,max(sample(x,replace=TRUE)))


# PARAMETRIC BOOTSTRAP
M <- max(x)
max.parboot <- replicate(1e4,max(runif(100,0,M)))

# Compare both histograms
layout(matrix(c(1,2),2,1))
H<-hist(max.boot)
Hp<-hist(max.parboot)

# P[hatthetaboot = hattheta]
sum(max.boot==M)/1e4
sum(max.parboot==M)/1e4

layout(matrix(c(1),1,1))
plot(ecdf(max.boot),verticals=TRUE,do.points=FALSE,xlim=c(0.9,theta))
xs <- seq(0,theta,length.out=500)
lines(xs,(xs/theta)^length(x),col="red",lwd=2)


