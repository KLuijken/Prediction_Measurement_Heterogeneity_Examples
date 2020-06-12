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
  
  # Estimate apparent performance (switch Maximum Likelihood versus Ridge)
  lp <- estimate_lp(data = data,
                    model = model,
                    method =  method,
                    validation_predictor = validation_predictor)
  store_internal   <- estimate_performance(lp = lp, Y = data[,"Y"])
  
  results_internal <- data.table(cal_large = store_internal$cal_large,
                                 cal_slope = store_internal$cal_slope,
                                 c_stat = store_internal$c_stat,
                                 Brier = store_internal$Brier,
                                 Brier_scaled = store_internal$Brier_scaled,
                                 R2 = estimate_R2(data =  data,
                                                  model =  model,
                                                  method = method,
                                                  lp = lp))
  
  # estimate optimism
  optimism <- results_internal - results_apparent
  colnames(optimism) <- paste0("opt_",colnames(results_internal))
  
  results_internal <- cbind(results_internal,
                            optimism,
                            seed = seed,
                            file_name = paste0("./results/analysis/",method,"/",
                                               method,"_internal_perf_",
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
                             t(calibrationslopex),
                            file_name = paste0("./results/analysis/calslope/",
                     method,"_calslope_x.rds")))
  save_result(calibrationslopex)
  
  calibrationslopey <- lowess(expit_lp,
                              data[,"Y"],
                              iter=0)$y[c(seq(from = 1, to = nrow(data),
                                              by = round(nrow(data)/20)),
                                          nrow(data))]
  calibrationslopey <- data.table(cbind(seed = seed, 
                             t(calibrationslopey),
                             file_name = paste0("./results/analysis/calslope/",
                                                method,"_calslope_y.rds")))
  save_result(calibrationslopey)

}

