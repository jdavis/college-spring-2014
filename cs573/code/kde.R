data(faithful)
# bandwidth selection according the solve-the-equation-plug-in rule
h <- bw.SJ(faithful[,2],method=c("ste"))
h
plot(density(faithful[,2],bw=h),main=" ",xlab="Time between eruptions")
