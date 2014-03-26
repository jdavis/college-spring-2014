library(ggplot2)

df <- data.frame(x = seq(-3,3,length.out=100))
df$y <- 2 + 3 * df$x + rnorm(100, sd = 0.5)
ggplot(data = df, aes(x = x, y = y)) +
     geom_smooth(method="lm",formula=y~x, se=FALSE,color="black",size=2)+
     geom_point(color="red",cex=3)

##########
T <- lm(y~x,data=df)
summary(T)