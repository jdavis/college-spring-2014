data(iris)

library(klaR)
m <- NaiveBayes(Species ~ ., data = iris)
pred<-predict(m,iris[,1:4])
table(iris[,5],pred$class)

# Use kernel density estimation with normal kernel
m <- NaiveBayes(Species ~ ., data = iris,usekernel=TRUE)
plot(m)
pred<-predict(m,iris[,1:4])
table(iris[,5],pred$class)

# Maybe better with LDA or QDA?
library(MVN)
# 1) Test MVN
HZ.test(as.matrix(iris[iris[,5]=="setosa",1:4]), cov = TRUE, plot = TRUE)
HZ.test(as.matrix(iris[iris[,5]=="versicolor",1:4]), cov = TRUE, plot = TRUE)
HZ.test(as.matrix(iris[iris[,5]=="virginica",1:4]), cov = TRUE, plot = TRUE)

#2) Homogeneity of covariance matrices
BoxMTest(iris[,1:4],iris[,5])

# So we choose: L or Q DA? 
m <- qda(Species ~ ., data = iris)
pred<-predict(m,iris[,1:4])
table(iris[,5],pred$class)
plot(m)

# nBayes vs qda on different example
library(MASS)
data(Cushings)
unk <- (1:27)[Cushings[,3]=="u"]
x <- log(Cushings[-unk,1:2])
labels <- factor(Cushings[-unk,3])
partimat(labels~.,data=x,method="naiveBayes",prec=50)
partimat(labels~.,data=x,method="qda",prec=50)