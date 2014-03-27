library(genridge) 

# Calculate Ridge estimator for a range of lambda's:0 -> 10
d <- read.table("diabetes.csv",header=TRUE,sep=",")
lambda<-seq(0,10,length.out=100)
L<-ridge(Y~.,data=d,lambda=lambda)

# Visualize how the coefficients shrink with increasing lambda
traceplot(L,X=c("lambda"))

# Tune the lambda according to GCV and plot
plot(lambda,L$GCV,pch=16,type="b",ylab="GCV",xlab=expression(lambda))

# Find minimum
M<-as.matrix(L$GCV)
abline(v=L$lambda[which.min(M)])

# Final result
traceplot(L,X=c("lambda"))
abline(v=L$kGCV)

