data(Cushings)
unk <- (1:27)[Cushings[,3]=="u"]
x <- log(Cushings[-unk,1:2])
labels <- factor(Cushings[-unk,3])

# Visualize data
plot(x,col=unclass(labels),pch=as.character(labels))

# Let's test first if teh class means are significantly different
res.manova<-manova(as.matrix(x)~labels)
summary(res.manova,test="Wilks")

# Assumptions for LDA and QDA
# 1) Does each class come from Multivariate normal?
library(MVN)
HZ.test(as.matrix(x[labels=="a",]), cov = TRUE, plot = TRUE)
HZ.test(as.matrix(x[labels=="b",]), cov = TRUE, plot = TRUE)
HZ.test(as.matrix(x[labels=="c",]), cov = TRUE, plot = TRUE)

#2) Homogeinity of var-covar matrices
BoxMTest(x,labels)

# We have to use QDA
# Visualize QDA
library(klaR)
library(glmnet)
partimat(labels~x$Pregnanetriol+x$Tetrahydrocortisone,data=x,method="qda",prec=500)

# Visualize LDA
partimat(labels~x$Pregnanetriol+x$Tetrahydrocortisone,data=x,method="lda",prec=500)