x <- matrix(rnorm(5000,0,1),nrow=125,ncol=40)
y <- 5+rnorm(5000,0,0.2)

df <- data.frame(x=x,y=y)
T<-lm(y~.,data=df)

# Roughly, if alpha=0.05, there will ba a probability of 5% that the t-test will
# be significant while the true beta_j=0
summary(T)