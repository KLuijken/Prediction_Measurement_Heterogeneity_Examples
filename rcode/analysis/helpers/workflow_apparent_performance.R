#------------------------------------------------------------------------------#
# Predictor measurement heterogeneity empirical
# K Luijken
#
# Workflow helper functions to obtain measures of apparent predictive performance
#------------------------------------------------------------------------------#

apparent_performance <- function(method,
                                 derivation_predictor, 
                                 validation_predictor,
                                 data){
  # Estimate model (switch Maximum Likelihood versus Ridge)
  model    <- estimate_model(data = data,
                             method = method,
                             derivation_predictor = derivation_predictor)
  
  # Estimate apparent performance (switch Maximum Likelihood versus Ridge)
  lp <- estimate_lp(data = data,
                          model = model,
                          method =  method,
                          validation_predictor = validation_predictor)
  store_apparent   <- estimate_performance(lp = lp, Y = data[,"Y"])
  
  results_apparent <- data.table(cal_large = store_apparent$cal_large,
                         cal_slope = store_apparent$cal_slope,
                         c_stat = store_apparent$c_stat,
                         Brier = store_apparent$Brier,
                         Brier_scaled = store_apparent$Brier_scaled,
                         R2 = estimate_R2(data =  data,
                                          model =  model,
                                          method = method,
                                          lp = lp))
  # Save measures and output
  saveRDS(results_apparent, file = paste0("./data/analysis/",method,"/apparent_perf_",
                                  derivation_predictor,".rds"))
  
  pdf(paste("./results/apparent_performance/",method,"_apparent_perf_",
            derivation_predictor,".pdf",sep = ""))
  invisible(val.prob(expit(lp),data$Y))
  dev.off()
  
  return(results_apparent)
}

