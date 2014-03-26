# Requires ggplot2
# pred = latent variables of classifier
# labels = true class labels (0 or 1)
ROC <- function(pred, labels){
  probsSort <- sort(pred, decreasing = TRUE, index.return = TRUE)
  val <- unlist(probsSort$x)
  idx <- unlist(probsSort$ix)  
  idx <- as.numeric(idx)
  labels<-as.numeric(labels)
  
  roc_y <- labels[idx]
  stack_x <- cumsum(roc_y == 0)/sum(roc_y == 0)
  stack_y <- cumsum(roc_y == 1)/sum(roc_y == 1)    
  
  auc <- sum((stack_x[2:length(roc_y)]-stack_x[1:length(roc_y)-1])*stack_y[2:length(roc_y)])
  
  c <- ggplot(data.frame(x=stack_x,y=stack_y), aes(y=y, x=x))
  c <- c + labs(title = substitute("ROC curve, auc" ==auc, list(auc=auc)))
  c + geom_line(colour = "blue",size=1)+xlab("False Positive Rate") +ylab("True Positive Rate") +
    xlim(0,1)+ ylim(0,1)+ geom_line(aes(x=c(0,0),y=c(0,y[1])),colour="blue",size=1)+
    geom_line(aes(x=c(0,1),y=c(0,1)),colour="gray",size=0.75,linetype="dashed")
}