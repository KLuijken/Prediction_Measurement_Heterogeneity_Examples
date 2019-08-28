
# Create descriptives of data


Bas.tab <- function(Data,Original){
  print(dfSummary(Original,round.digits=3),file=file.path(filepath,"baseline.html"))
  print(dfSummary(Original[Data[,"Y"]==0,],round.digits=3),file=file.path(filepath,"baseline.html"),append=T)
  print(dfSummary(Original[Data[,"Y"]==1,],round.digits=3),file=file.path(filepath,"baseline.html"),append=T)
  print(dfSummary(Data[Data[,"Y"]==0,],round.digits=3),file=file.path(filepath,"baseline.html"),append=T)
  print(dfSummary(Data[Data[,"Y"]==1,],round.digits=3),file=file.path(filepath,"baseline.html"),append=T)
  
  # Correlation matrix
  correlation <- cor(Data,method= "pearson")
  print(xtable(correlation,digits=5),file=file.path(filepath,"Corr_Tex.txt"), compress=F)
  
  # Partial correlation X-W
  mm1  <- lm(reformulate(termlabels = c('Y',covariates), response = 'X'),data=Data)
  res1 <- mm1$residuals
  mm2  <- lm(reformulate(termlabels = c('Y',covariates), response = 'W'),data=Data)
  res2 <- mm2$residuals
  p.correlation <- cor(res1,res2)
  
  # Outcome proportion
  pout <- sum(Data$Y)/nrow(Data)
  write.table(c(p.correlation,pout),file=file.path(filepath,"PCorr_Tex.txt"), row.names = c("partial correlation","outcome proportion"))
  
  # Scatterplot X-W
  
  tiff(paste(filepath,"/PlotXW.tiff", sep=""), width = 4, height = 4, units = 'in',res=200)
  plot(Data$X,
       Data$W, 
       ylab = paste(label(Data$X)),
       xlab = paste(label(Data$W)),
       las=1,
       cex.lab=0.75,
       cex.axis=0.75)
  abline(lm(Data$W~Data$X),col="black")
  dev.off()
  
}
