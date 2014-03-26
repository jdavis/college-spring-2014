advertising <- read.table("Advertising.csv",header=TRUE,sep=",")
advertising <- advertising[,-1]

# Fit linear model
T <- lm(Sales~TV+Radio+Newspaper,data=advertising)
summary(T)

pairs(advertising)