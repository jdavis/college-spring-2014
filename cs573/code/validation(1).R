library (ISLR)
attach(Auto)
library(ggplot2)
##################
#   Validation   #
##################
set.seed(12)
#sample at random 196 points out of 392
train <- sample (392 ,196)

# fit linear model only using training samples
lm.fit <- lm(mpg~horsepower,data=Auto,subset =train)

# calculate the MSE of the 196 observations in the validation set
mean((mpg -predict (lm.fit ,Auto))[-train ]^2)

# Same for quadratic and cubic regression
lm.fit2<-lm(mpg~poly(horsepower ,2) ,data=Auto ,subset =train )
mean((mpg -predict (lm.fit2 ,Auto))[-train ]^2)

lm.fit3<-lm(mpg~poly(horsepower ,3) ,data=Auto ,subset =train )
mean((mpg -predict (lm.fit3 ,Auto))[-train ]^2)

##################
#     LOOCV      #
##################
library (boot)
# just linear regression
glm.fit <- glm(mpg~horsepower ,data=Auto)
cv.err <- cv.glm(Auto ,glm.fit)

# LOOCV: different orders
cv.error<-rep (0,10)
for (i in 1:10){
   glm.fit<-glm(mpg~poly(horsepower ,i),data=Auto)
   cv.error[i]<-cv.glm(Auto,glm.fit)$delta [1]
   }

df <- data.frame(Degree = c(1:10), CV = cv.error)
g <- ggplot(df, aes(x = Degree, y = CV))
g + geom_line(col="red",lwd=1)+geom_point(pch=16,col="red",cex=5)

###############################
# k-fold CV: different orders #
###############################
set.seed(24)
cv.error.10 <- rep (0 ,10)
for (i in 1:10) {
   glm.fit<-glm(mpg~poly(horsepower ,i),data=Auto)
   cv.error.10[i]=cv.glm (Auto ,glm.fit ,K=10) $delta [1]
   }
df <- data.frame(Degree = c(1:10), CV = cv.error.10)
g <- ggplot(df, aes(x = Degree, y = CV))
g + geom_line(col="red",lwd=1)+geom_point(pch=16,col="red",cex=5)

#####################
# k-NN on Iris data #
#####################
library(class)
data(iris)
set.seed(22)
X <- as.matrix(iris[,1:4])
labels <- as.numeric(iris[,5])-1
cv.knn <- 0
for (i in 1:30){
  # LOOCV for k-NN
  cv <- knn.cv(X,labels,k=i,use.all=TRUE) 
  cv.knn[i] <- sum(cv!=labels)
}
df <- data.frame(k = c(1:30), CV = cv.knn)
g <- ggplot(df, aes(x = k, y = CV))
g + geom_line(col="red",lwd=1)+geom_point(pch=16,col="red",cex=5)+ 
  ylab("CV (# misclassifications)")
  