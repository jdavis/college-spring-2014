household <- read.table("household.dat",header=TRUE,sep=" ")

x <- as.matrix(household[,1])
labels <- household[,2]

# LDA
L <- lda(x,labels)
L

stripchart(x~labels)
abline(v=(41944.44+47509.09)/2)

# Prediction
pred <-predict(L,x)
pred

# ROC
library(ggplot2)
ROC(pred$x,labels)

# LDA histogram
ldahist(pred$x,labels)