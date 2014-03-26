library(leaps)
# data is in matrix format, data frame is needed
data(state) 
state<-data.frame(state.x77,row.names=state.abb,check.names=T)
# Display only the 2 best models for each number of subsets
leaps<-regsubsets(Life.Exp~.,data=state,nvmax=dim(state)[2]-1)
S<-summary(leaps)

#plotting results and highlight best choice
par(mfrow =c(2,2))
plot(S$rss ,xlab=" Number of Variables ",ylab=" RSS",type="l",lwd=2)
w<-which.min (S$rss)
points(w,S$rss[w], col ="red",cex =2, pch =20)

plot(S$adjr2 ,xlab =" Number of Variables ",ylab=" Adjusted RSq",type="l",lwd=2)
w<-which.max (S$adjr2)
points(w,S$adjr2[w], col ="red",cex =2, pch =20)

plot(S$cp ,xlab =" Number of Variables ",ylab="Cp",type="l",lwd=2)
w<-which.min (S$cp)
points(w,S$cp[w], col ="red",cex =2, pch =20)

plot(S$bic ,xlab =" Number of Variables ",ylab="BIC",type="l",lwd=2)
w<-which.min (S$bic)
points(w,S$bic[w], col ="red",cex =2, pch =20)

# another way
plot(leaps,scale="bic")
plot(leaps,scale="Cp")