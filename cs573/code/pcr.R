library(pls)
library(ISLR)
set.seed (1)

# Load data
data(Hitters)

# Fit PCR model
pcr.fitfull<-pcr(Salary~., data=Hitters ,scale =TRUE,validation ="LOO")
summary(pcr.fitfull)

# Show Cv plot
plot(RMSEP(pcr.fitfull), legendpos = "topright")

# Fit complete model with 6 components
pcr.fit<-pcr(Salary~., data=Hitters ,scale =TRUE,ncomp =6)
summary(pcr.fit)

# Plot % explained variances
plot(explvar(pcr.fitfull),type="b",xlab="No. of Components")
explvar(pcr.fitfull)

# Final regression coef.
coef(pcr.fit,intercept=TRUE)
