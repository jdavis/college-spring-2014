library(leaps)
library(DAAG)
# data is in matrix format, data frame is needed
data(state) 
state<-data.frame(state.x77,row.names=state.abb,check.names=T)

#############
# Method ####
#############
# Start with smallest model G
G <- lm(Life.Exp~1,data=state)
press(G)
# step 1
press(update(G, ~ . + Area))
press(update(G, ~ . + Population))
press(update(G, ~ . + Income))
press(update(G, ~ . + Illiteracy))
press(update(G, ~ . + Murder))
press(update(G, ~ . + HS.Grad))
press(update(G, ~ . + Frost))

# Step 2: add Murder
G <- update(G, ~ . + Murder)

press(update(G, ~ . + Area))
press(update(G, ~ . + Population))
press(update(G, ~ . + Income))
press(update(G, ~ . + Illiteracy))
press(update(G, ~ . + HS.Grad))
press(update(G, ~ . + Frost))
press(update(G, ~ . + Area))

# Step 3: add Population
G <- update(G, ~ . + Population)

press(update(G, ~ . + Area))
press(update(G, ~ . + Income))
press(update(G, ~ . + Illiteracy))
press(update(G, ~ . + HS.Grad))
press(update(G, ~ . + Frost))
press(update(G, ~ . + Area))

# Step 4: add HS.Grad
G <- update(G, ~ . + HS.Grad)

press(update(G, ~ . + Area))
press(update(G, ~ . + Income))
press(update(G, ~ . + Illiteracy))
press(update(G, ~ . + Frost))
press(update(G, ~ . + Area))

# Step 5: add Frost (4th predictor)
G <- update(G, ~ . + Frost)
press(G)

press(update(G, ~ . + Area))
press(update(G, ~ . + Income))
press(update(G, ~ . + Illiteracy))
press(update(G, ~ . + Area))

# STOP: CANNOT REDUCE PRESS statistic
# Final Model
summary(G)

############
# Automatic#
############
leaps.fwd<-regsubsets(Life.Exp~.,data=state ,nvmax =7,method ="forward")
S<-summary(leaps.fwd)
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
plot(leaps.fwd,scale="bic")
plot(leaps.fwd,scale="Cp")

# Another method
library(MASS)
Gn <- lm(Life.Exp~1.,data=state) # null model
step <- stepAIC(Gn,direction="forward",scope=list(upper = ~Population+Income+Illiteracy+Murder+HS.Grad+Frost+Area, lower = ~1))