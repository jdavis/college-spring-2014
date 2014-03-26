x <- seq(-3,3,length.out=200)
y <- 2+3*x+rnorm(200,0,1)
T1 <- lm(y~x)
T2 <- lm(y~poly(x,2)) # Fit a quadratic model Y=a+bx+cx^2

library(ggplot2)
df <- data.frame(x = x, y=y)
ggplot(data = df, aes(x = x, y = y)) +
  geom_smooth(method="lm",formula=y~x, se=FALSE,color="green",size=1)+
  geom_smooth(method="lm",formula=y~poly(x,2), se=FALSE,color="red",size=1)+
  geom_point(color="black",cex=3)

summary(T2)

