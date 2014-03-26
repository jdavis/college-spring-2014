household <- read.table("household.dat",header=TRUE,sep=" ")
m<-glm(hownership~income,data=household,family=binomial)
m

# Plot the logistic regression result
library(ggplot2)
c <- ggplot(household, aes(y=hownership, x=income))
c + stat_smooth(method="glm", family="binomial",se=FALSE,lwd=1) +
  geom_point(cex=5)

# var-covar matrix of the estimated parameters
vcov(m)

# LACK OF FIT
# Now fit the NULL model m0
m0<-glm(hownership~1,data=household,family=binomial())
anova(m0,m,test="LRT")

#predicting an observation
c<-predict(m,data.frame(income=50000),type=c("response"))

# confusion matrix
pred<-predict(m,data.frame(income=household$income),type=c("response"))
# Make decision
predclass <- rep(0,dim(household)[1])
predclass[pred>=.5] = 1

# Make matrix
table(household$hownership,predclass)


# Make a plot of the classifier
plot(household$income,household$hownership,xlab="Income", ylab="Class labels")
cut<- -m$coeff[[1]]/m$coeff[[2]]
abline(v=cut)