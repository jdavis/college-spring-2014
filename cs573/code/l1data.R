#set.seed(10131986) # for reproducibility
#install.packages("quantreg")
#library(quantreg)
#library(MASS)
l1data <- function(n1, n2){
   data <- mvrnorm(n=n1,mu=c(0, 0),Sigma=matrix(c(1, .9, .9, 1), ncol=2))
   # generate 20 'bad' observations
   data <- rbind(data, mvrnorm(n=n2,mu=c(1.5, -1.5), Sigma=.2*diag(c(1, 1))))
   data <- data.frame(data)
   names(data) <- c("X", "Y")
   ind <- c(rep(1, n1),rep(2, n2))
   plot(Y ~ X, data, pch=c("x", "o")[ind], col=c("black", "red")[ind], main=substitute(list(N[1] == n1, N[2] == n2), list(n1=n1, n2=n2)))
   summary(r1 <-rq(Y ~ X, data=data, tau=0.5))
   abline(r1, lwd=2)
   abline(lm(Y ~ X, data), lty=2, lwd=2, col="red")
   abline(lm(Y ~ X, data, subset=1:n1), lty=3, lwd=2, col="blue")
   legend("topleft", c("L1","OLS","OLS on good"), inset=0.02, lty=1:3, lwd=2, col=c("black", "red", "blue"), cex=.9)
}

par(mfrow=c(2, 2))
l1data(200, 5)
l1data(200, 10)
l1data(200, 20)
l1data(200, 40)