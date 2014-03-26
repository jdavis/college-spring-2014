C1 <- mvrnorm(60, c(0,4), matrix(c(1,.1,.1,1),2))
C2 <- mvrnorm(60, c(-2,-2), matrix(c(1,.1,.1,1),2))
C3 <- mvrnorm(60, c(3,0), matrix(c(1,.1,.1,1),2))
C4 <- rbind(C1,C2,C3)
C <- data.frame(X1=C4[,1], X2=C4[,2])
C.class <- as.factor(rep(c("a","b","c"),each=60))

L <- lda(C,C.class)
pred <-predict(L,C)

# Visualize the classifier
library(klaR)
partimat(C.class~C[,2]+C[,1],data=C,method="lda",prec=1000)

# Make prediction
pred <-predict(L,C)

#lda histogram for 1nd discriminant function
ldahist(pred$x[,1],C.class)

#lda histogram for 2nd discriminant function
ldahist(pred$x[,2],C.class)

#confusion matrix
table(C.class,pred$class)

# Correlations between predictors and discriminant values indicate which predictor is most related to discriminant function
cor(C[,1],pred$x[,1])
cor(C[,2],pred$x[,1])

# Wilks' lambda
res.manova<-manova(as.matrix(C)~C.class)
summary(res.manova,test="Wilks")

# Test multivariate normality
library(MVN)
HZ.test(as.matrix(C[C.class=="a",]), cov = TRUE, plot = TRUE)
HZ.test(as.matrix(C[C.class=="b",]), cov = TRUE, plot = TRUE)
HZ.test(as.matrix(C[C.class=="c",]), cov = TRUE, plot = TRUE)

# Testing for homogeneity of covariance matrices
BoxMTest(C,C.class)