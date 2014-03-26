D1 <- mvrnorm(80, c(0,-1), matrix(c(1.5,.8,.8,1.5),2))
D2 <- mvrnorm(80, c(-1,2), matrix(c(.25,-.2,-.2,.25),2))
D3 <- rbind(D1,D2)
D <- data.frame(X1=D3[,1], X2=D3[,2] )
D.class <- as.factor(rep(c("a","b"),each=80))

# LDA example
D.lda <- lda(D, D.class)
D.pred.l <- predict(D.lda, D)
table(D.class,D.pred.l$class)
# Visualize the classifier
library(klaR)
partimat(D.class~D[,2]+D[,1],data=D,method="lda",prec=500)

# QDA example
D.qda <- qda(D, D.class)
D.pred.l <- predict(D.qda, D)
table(D.class,D.pred.l$class)
# Visualize the classifier
partimat(D.class~D[,2]+D[,1],data=D,method="qda",prec=500)

# AUC LDA
labels <- as.numeric(D.class)-1
pred.lda <- predict(D.lda, D)$posterior[,2]
auc(labels,pred.lda)

# AUC LDA
pred.qda <- predict(D.qda, D)$posterior[,2]
auc(labels,pred.qda)