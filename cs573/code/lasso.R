library(glmnet) 

data(state)
state<-data.frame(state.x77,row.names=NULL,check.names=T)

# Calculate lasso estimator for a range of lambda's:10^-5 -> 10^1
lambdagrid<-10^(seq(-5,1,length.out=500))
y <- state$Life.Exp
# Remove the Y
x <- scale(state[,-4])

# perform lasso: i.e. set alpha=1 (ridge: alpha=0)
lasso<-glmnet(x,y,alpha=1,lambda=lambdagrid)
# plot against against log(lambda)
plot(lasso,xvar="lambda")
cv.lasso <- cv.glmnet(x,y,alpha=1,lambda=lambdagrid)
plot(cv.lasso)
lambda <- cv.lasso$lambda.min
lambda

# coefficients
coef <- predict(lasso,type="coefficients",s=lambda)


###
# Another way
library(lars)
X <- scale(state[,-4])
# computes for entire regularization path
m <- lars(x,state[,4],type="lasso")
# plot of ||beta_lasso||_1 / ||\beta_OLS||_1
mlambda<-cv.lars(X,state[,4],K=50,trace=FALSE,type="lasso")
m
plot(m)
plot(0:7,m$Cp,type="b",xlab=expression(paste("Number ", beta, ">0")),ylab="Cp")
# What are the coefficients
coef(m)[which.min(m$Cp),]