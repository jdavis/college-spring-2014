library(leaps)
library(DAAG)
library(MASS)
# data is in matrix format, data frame is needed
data(state) 
state<-data.frame(state.x77,row.names=state.abb,check.names=T)

# Method 1
leaps<-regsubsets(Life.Exp~.,data=state ,nvmax =7,method ="seqrep")
S<-summary(leaps)
plot(leaps,scale="bic")
plot(leaps,scale="Cp")

# Method 2
G<-lm(Life.Exp~1,data=state)
step <- stepAIC(G, direction="both",scope=list(upper = ~Population+Income+Illiteracy+Murder+HS.Grad+Frost+Area, lower = ~1))