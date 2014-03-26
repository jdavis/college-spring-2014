# visulaize collinearity
# Load credit data set
credit <- read.table("Credit.csv",header=TRUE,sep=",")
# remove first column
credit <- credit[,-1]
# Make a pairs plot (see p. 83 of the textbook)
pairs(credit)

# See difference in estimates: Rating and Limit are collinear
# Observe the std errors
T1 <- lm(Balance~Age+Limit,data=credit)
T2coll <- lm(Balance~Rating+Limit,data=credit)
summary(T1)
summary(T2coll)

# Do a regression of Balance on Age, Rating and Limit
T2coll <- lm(Balance~Age+Rating+Limit,data=credit)
library(car)
vif(T2coll)

# Drop problematic variable: e.g. Rating
T2collsol <- lm(Balance~Age+Limit,data=credit)
vif(T2collsol)