
### Create spaghettiplots of calibration slopes in bootstrap resampling



SpagGraph <- function(perfmeasure,B){
  plot(NULL,
       xlab="Predicted outcome", 
       ylab="Observed outcome",
       xlim=c(0,1), 
       ylim=c(0,1), 
       main = NULL,
       cex.lab=1, 
       cex.axis=1,
       cex.main=1,
       las=1)
  abline(0,1, lty=2)
  
  for(j in 1:B){
    data2 <- data.frame(perfmeasure[j,,1],perfmeasure[j,,2])
    colnames(data2) <- c("x","y")
    lines(lowess(data2), col = alpha("black",0.01))
  }
}
