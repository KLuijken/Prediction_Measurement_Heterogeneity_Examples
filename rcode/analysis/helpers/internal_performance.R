#------------------------------------------------------------------------------#
# Predictor measurement heterogeneity empirical
# K Luijken
#
# Workflow helper functions to obtain measures of internal predictive performance
#------------------------------------------------------------------------------#

# Helpers ----
#------------------------------------------------------------------------------#

# Save measures and output
saveRDS(results_apparent, file = paste0("./results/analysis/",method,"/",method,"_internal_perf_",
                                        derivation_predictor,".rds"))

save_result <- function(result){
  file_name <- result[['file_name']]
  # append new result to old results if file exists
  if (file.exists(file_name)){
    con <- file(file_name)
    while (isOpen(con)){
      Sys.sleep(2)
    }
    open(con)
    results_in_file <- readRDS(file_name)
    new_results <- rbind(results_in_file, 
                         result)
    rownames(new_results) <- NULL
    saveRDS(new_results, file = file_name)
    close(con)
  } else{ #create new file
    saveRDS(result, file = file_name)}
}


# Summarize

# Workhorse function internal performance ----
#------------------------------------------------------------------------------#

internal_performance <- function(method,
                                 derivation_predictor, 
                                 validation_predictor,
                                 data,
                                 results_apparent,
                                 seed){
  # Estimate model (switch Maximum Likelihood versus Ridge)
  model    <- estimate_model(data = data,
                             method = method,
                             derivation_predictor = derivation_predictor)
  
  # Estimate apparent performance (switch Maximum Likelihood versus Ridge)
  lp <- estimate_lp(data = data,
                    model = model,
                    method =  method,
                    validation_predictor = validation_predictor)
  store_internal   <- estimate_performance(lp = lp, Y = data[,"Y"])
  
  results_internal <- data.frame(cal_large = store_internal$cal_large,
                                 cal_slope = store_internal$cal_slope,
                                 c_stat = store_internal$c_stat,
                                 Brier = store_internal$Brier,
                                 Brier_scaled = store_internal$Brier_scaled,
                                 R2 = estimate_R2(data =  data,
                                                  model =  model,
                                                  method = method,
                                                  lp = lp))
  
  optimism <- results_internal - results_apparent
  colnames(optimism) <- paste0("opt_",colnames(results_internal))
  
  results_internal <- cbind(results_internal,
                            optimism,
                            seed = seed,
                            file_name = paste0("./results/analysis/",method,"/",
                                               method,"_internal_perf_",
                                               derivation_predictor,".rds"))
  
  save_result(results_internal)
  
  expit_lp <- expit(lp)
  calibrationslopex <- lowess(expit_lp,
                             data[,"Y"],
                             iter=0)$x[c(seq(from = 1, to = nrow(data),
                                             by = round(nrow(data)/20)),
                                         nrow(data))]
  calibrationslopex <- cbind(seed = seed, 
                            calibrationslope,
                            file_name = paste0("./results/analysis/calibrationslope/",
                     method,"_calibrationslope_xaxis.rds"))
  save_result(calibrationslopex)
  
  calibrationslopey <- lowess(expit_lp,
                              data[,"Y"],
                              iter=0)$y[c(seq(from = 1, to = nrow(data),
                                              by = round(nrow(data)/20)),
                                          nrow(data))]
  calibrationslopey <- cbind(seed = seed, 
                             calibrationslope,
                             file_name = paste0("./results/analysis/calibrationslope/",
                                                method,"_calibrationslope_yaxis.rds"))
  save_result(calibrationslopey)

}

