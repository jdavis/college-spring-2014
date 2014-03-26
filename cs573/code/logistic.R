household <- read.table("household.dat",header=TRUE,sep=" ")
m<-glm(hownership~income,data=household,family=binomial)
m