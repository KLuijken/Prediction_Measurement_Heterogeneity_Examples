#------------------------------------------------------------------------------#
# Predictor measurement heterogeneity empirical
# K Luijken
#
# Workflow helper functions to obtain measures of internal predictive performance
#------------------------------------------------------------------------------#

# Helpers ----
#------------------------------------------------------------------------------#

save_result <- function(results){
  filepath <- results$file_name
  
  if (file.exists(filepath)){
    con <- file(filepath)
    while (isOpen(con)){
      Sys.sleep(2)
    }
  open(con)
  results_in_file <- readRDS(filepath)
  new_results <- rbind(results_in_file, 
                       results)
  rownames(new_results) <- NULL
  saveRDS(new_results, file = filepath)
  close(con)
  } else{ #create new file
    saveRDS(results, file = filepath)}
  
}


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
  
  # Estimate performance in bootstrap sample (switch Maximum Likelihood versus Ridge)
  lp <- estimate_lp(data = data,
                    model = model,
                    method =  method,
                    validation_predictor = validation_predictor)
  store_bootstrap   <- estimate_performance(lp = lp, Y = data[,"Y"])
  
  results_bootstrap <- data.table(cal_large = store_bootstrap$cal_large,
                                 cal_slope = store_bootstrap$cal_slope,
                                 c_stat = store_bootstrap$c_stat,
                                 Brier = store_bootstrap$Brier,
                                 Brier_scaled = store_bootstrap$Brier_scaled,
                                 R2 = estimate_R2(data =  data,
                                                  model =  model,
                                                  method = method,
                                                  lp = lp))
  
  # estimate optimism
  optimism <- results_bootstrap - results_apparent
  colnames(optimism) <- paste0("opt_",colnames(results_bootstrap))
  
  results_internal <- cbind(results_bootstrap,
                            optimism,
                            seed = seed,
                            file_name = paste0("./data/analysis/",method,
                                               "/internal_perf_",
                                               derivation_predictor,
                                               validation_predictor,".rds"))
  
  save_result(results_internal)
  
  # store calibration slope coordinates for plotting
  expit_lp <- expit(lp)
  calibrationslopex <- lowess(expit_lp,
                             data[,"Y"],
                             iter=0)$x[c(seq(from = 1, to = nrow(data),
                                             by = round(nrow(data)/20)),
                                         nrow(data))]
  calibrationslopex <- data.table(cbind(seed = seed, 
                                        t(calibrationslopex)))
  calibrationslopex$file_name <- paste0("./data/analysis/calslope/",
                                                    method,"_calslope_",
                                        derivation_predictor, 
                                        validation_predictor,
                                        "_xaxis.rds")
  save_result(calibrationslopex)
  
  calibrationslopey <- lowess(expit_lp,
                              data[,"Y"],
                              iter=0)$y[c(seq(from = 1, to = nrow(data),
                                              by = round(nrow(data)/20)),
                                          nrow(data))]
  calibrationslopey <- data.table(cbind(seed = seed, 
                             t(calibrationslopey)))
  calibrationslopey$file_name <- paste0("./data/analysis/calslope/",
                                                method,"_calslope_",
                                        derivation_predictor, 
                                        validation_predictor,
                                        "_yaxis.rds")
  save_result(calibrationslopey)

}

