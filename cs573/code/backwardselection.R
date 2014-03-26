library(leaps)
library(DAAG)
# data is in matrix format, data frame is needed
data(state) 
state<-data.frame(state.x77,row.names=state.abb,check.names=T)

#############
# Method ####
#############
# Start with FULL model G
G <- lm(Life.Exp~.,data=state)
press(G)
# step 1
press(update(G, ~ . - Area))
press(update(G, ~ . - Population))
press(update(G, ~ . - Income))
press(update(G, ~ . - Illiteracy))
press(update(G, ~ . - Murder))
press(update(G, ~ . - HS.Grad))
press(update(G, ~ . - Frost))

# Step 2: delete Area
G <- update(G, ~ . - Area)
press(G) # to beat

press(update(G, ~ . - Population))
press(update(G, ~ . - Income))
press(update(G, ~ . - Illiteracy))
press(update(G, ~ . - Murder))
press(update(G, ~ . - HS.Grad))
press(update(G, ~ . - Frost))

# Step 3: delete Income
G <- update(G, ~ . - Income)
press(G) # to beat

press(update(G, ~ . - Population))
press(update(G, ~ . - Illiteracy))
press(update(G, ~ . - Murder))
press(update(G, ~ . - HS.Grad))
press(update(G, ~ . - Frost))

# Step 4: delete Illiteracy
G <- update(G, ~ . - Illiteracy)
press(G) # to beat

press(update(G, ~ . - Population))
press(update(G, ~ . - Murder))
press(update(G, ~ . - HS.Grad))
press(update(G, ~ . - Frost))

# STOP: CANNOT REDUCE PRESS statistic
# Final Model
summary(G)

############
# Automatic#
############
leaps.bwd<-regsubsets(Life.Exp~.,data=state ,nvmax =7,method ="backward")
S<-summary(leaps.bwd)
#plotting results and highlight best choice
par(mfrow =c(2,2))
plot(S$rss ,xlab=" Number of Variables ",ylab=" RSS",type="l",lwd=2)
w<-which.min(S$rss)
points(w,S$rss[w], col ="red",cex =2, pch =20)

plot(S$adjr2 ,xlab =" Number of Variables ",ylab=" Adjusted RSq",type="l",lwd=2)
w<-which.max(S$adjr2)
points(w,S$adjr2[w], col ="red",cex =2, pch =20)

plot(S$cp ,xlab =" Number of Variables ",ylab="Cp",type="l",lwd=2)
w<-which.min(S$cp)
points(w,S$cp[w], col ="red",cex =2, pch =20)

plot(S$bic ,xlab =" Number of Variables ",ylab="BIC",type="l",lwd=2)
w<-which.min(S$bic)
points(w,S$bic[w], col ="red",cex =2, pch =20)

# another way
plot(leaps.bwd,scale="bic")
plot(leaps.bwd,scale="Cp")

#Another method
library(MASS)
Gf <- lm(Life.Exp~.,data=state) # Full model
step <- stepAIC(Gf, direction="backward")