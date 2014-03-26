#decrease len if you have little memory.
predplot <- function(object, data, class, len = 100, ...)
{
  plot(data,col=unclass(class),pch=unclass(class))
  xp <- seq(min(data[,1]),max(data[,1]),length=len)
  yp <- seq(min(data[,2]),max(data[,2]),length=len)
  grid <- expand.grid(xp,yp)
  colnames(grid) <- colnames(data)
  Z <- predict(object, grid, ...);
  zc <- as.numeric(Z$class)
  #browser()
  for (i in unique(zc))
  {zp <- Z$post[,i] - apply(as.matrix(Z$post[,-i]),1,max)
   contour(xp, yp, matrix(zp, len),
           add = T, levels = 0, drawlabels = F)}
  invisible()
}