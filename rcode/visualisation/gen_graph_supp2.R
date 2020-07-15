#------------------------------------------------------------------------------#
# Predictor measurement heterogeneity empirical
# K Luijken
#
# Generate smooth calibration plots supplementary file 2
#------------------------------------------------------------------------------#

generate_spaghettiplot <- function(perfmeasurex, perfmeasurey,B){
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
    data2 <- data.frame("x" = as.numeric(perfmeasurex[j,!c("seed","file_name")]),
                        "y" = as.numeric(perfmeasurey[j,!c("seed","file_name")]))
    lines(lowess(data2), col = alpha("black",0.01))
  }
}
