#------------------------------------------------------------------------------#
# Predictor measurement heterogeneity empirical
# K Luijken
#
# Generate tables
#------------------------------------------------------------------------------#

# Helpers ----
#------------------------------------------------------------------------------#

process_results_homogeneity <- function(method, derivation_predictor, validation_predictor, data){
  # Load results
  bootstrap <- readRDS(paste0("./results/analysis/",method,
                     "/internal_perf_",
                     derivation_predictor,
                     validation_predictor,".rds"))
  
  apparent  <- readRDS(paste0("./results/analysis/",method,
                        "/apparent_perf_",
                        derivation_predictor,".rds"))
  
  # Obtain measures averaged over bootstrap
  # Median for calibration measures (skewed performance measure)
  # Mean for c-stat and Brier score. Subtract optimism from apparent measure (optimism correction)
  point_estimates <- data.frame(cal_large = median(bootstrap$cal_large),
                                cal_slope = median(bootstrap$cal_slope),
                                c_stat = apparent$c_stat - mean(bootstrap$opt_c_stat),
                                Brier_scaled = apparent$Brier_scaled - 
                                  mean(bootstrap$opt_Brier_scaled))
  
  point_estimates <- round(point_estimates, digits = 2)
  
  # Obtain confidence intervals throug percentiles
  # Correct c-statistic and Brier score for optimism
  confidence_intervals_lower <- data.frame(cal_large = quantile(bootstrap$cal_large, 0.025, names = F),
                                           cal_slope = quantile(bootstrap$cal_slope, 0.025, names = F),
                                           c_stat = quantile(bootstrap$c_stat, 0.025, names = F) - 
                                             mean(bootstrap$opt_c_stat),
                                           Brier_scaled = quantile(bootstrap$Brier_scaled, 0.025, names = F) - 
                                             mean(bootstrap$opt_Brier_scaled))
  
  confidence_intervals_lower <- round(confidence_intervals_lower, digits = 2)
  
  confidence_intervals_upper <- data.frame(cal_large = quantile(bootstrap$cal_large, 0.975, names = F),
                                           cal_slope = quantile(bootstrap$cal_slope, 0.975, names = F),
                                           c_stat = quantile(bootstrap$c_stat, 0.975, names = F) - 
                                             mean(bootstrap$opt_c_stat),
                                           Brier_scaled = quantile(bootstrap$Brier_scaled, 0.975, names = F) - 
                                             mean(bootstrap$opt_Brier_scaled))
  
  confidence_intervals_upper <- round(confidence_intervals_upper, digits = 2)
  
  return(list(point_estimates = point_estimates,
              confidence_intervals_lower = confidence_intervals_lower,
              confidence_intervals_upper = confidence_intervals_upper))
}

process_results_heterogeneity <- function(method, derivation_predictor, validation_predictor, data){
  # Load results
  heterogeneity <- readRDS(paste0("./results/analysis/",method,
                              "/internal_perf_",
                              derivation_predictor,
                              validation_predictor,".rds"))
  
  homogeneity   <- readRDS(paste0("./results/analysis/",method,
                                 "/internal_perf_",
                                 derivation_predictor,
                                 derivation_predictor,".rds"))
  
  # Obtain measures averaged over bootstrap
  # Median for calibration measures (skewed performance measure)
  # Mean for c-stat and Brier score. Subtract optimism from apparent measure (optimism correction)
  point_estimates <- data.frame(cal_large = median(heterogeneity$cal_large),
                                cal_slope = median(heterogeneity$cal_slope),
                                c_stat = mean(heterogeneity$c_stat - homogeneity$c_stat),
                                Brier_scaled = mean(heterogeneity$Brier_scaled - 
                                                      homogeneity$Brier_scaled))
  
  point_estimates <- round(point_estimates, digits = 3)
  
  # Obtain confidence intervals throug percentiles
  # Correct c-statistic and Brier score for optimism
  confidence_intervals_lower <- data.frame(cal_large = quantile(heterogeneity$cal_large, 0.025, names = F),
                                           cal_slope = quantile(heterogeneity$cal_slope, 0.025, names = F),
                                           c_stat = quantile(heterogeneity$c_stat - homogeneity$c_stat,
                                                             0.025, names = F),
                                           Brier_scaled = quantile(heterogeneity$Brier_scaled - 
                                                                     homogeneity$Brier_scaled, 0.025, names = F))
  
  confidence_intervals_lower <- round(confidence_intervals_lower, digits = 3)
  
  confidence_intervals_upper <- data.frame(cal_large = quantile(heterogeneity$cal_large, 0.975, names = F),
                                           cal_slope = quantile(heterogeneity$cal_slope, 0.975, names = F),
                                           c_stat = quantile(heterogeneity$c_stat - homogeneity$c_stat,
                                                             0.975, names = F),
                                           Brier_scaled = quantile(heterogeneity$Brier_scaled - 
                                                                     homogeneity$Brier_scaled, 0.975, names = F))
  
  confidence_intervals_upper <- round(confidence_intervals_upper, digits = 3)
  
  return(list(point_estimates = point_estimates,
              confidence_intervals_lower = confidence_intervals_lower,
              confidence_intervals_upper = confidence_intervals_upper))
}

# Generate table measurement homogeneity  ----
#------------------------------------------------------------------------------#

generate_one_row_homogeneity <- function(method, derivation_predictor, validation_predictor, data){
  performance <- process_results_homogeneity(method = method,
                                  derivation_predictor = derivation_predictor,
                                  validation_predictor = validation_predictor,
                                  data = data)

  result_row <- data.frame(Event_fraction = nrow(data[data$Y==1,])/nrow(data),
                           Measurement_strategy = paste0(Hmisc::label(data[,derivation_predictor]),
                                                         ifelse(derivation_predictor == "X",
                                                                " (preferred)",
                                                                " (pragmatic)")),
                           Mean_value_measurement = round(mean(data[,derivation_predictor]),
                                                          digits = 2),
                           Standard_deviation_measurement = round(
                             sd(data[,derivation_predictor]), digits = 2),
                           C_statistic = paste0(performance$point_estimates$c_stat,
                                                " (95% CI,",
                                                performance$confidence_intervals_lower$c_stat,
                                                "; ",
                                                performance$confidence_intervals_upper$c_stat,
                                                ")"),
                           Scaled_Brier_score = paste0(performance$point_estimates$Brier_scaled,
                                                       " (95% CI,",
                                                       performance$confidence_intervals_lower$Brier_scaled,
                                                       "; ",
                                                       performance$confidence_intervals_upper$Brier_scaled,
                                                       ")"))
  return(result_row)
}


# Generate table measurement heterogeneity  ----
#------------------------------------------------------------------------------#

generate_one_row_heterogeneity <- function(method, derivation_predictor, validation_predictor, data){
  performance <- process_results_heterogeneity(method = method,
                                 derivation_predictor = derivation_predictor,
                                 validation_predictor = validation_predictor,
                                 data = data)
  
  result_row <- data.frame(Measurement_strategy_at_derivation = Hmisc::label(data[,derivation_predictor]),
                           Measurement_strategy_at_validation = Hmisc::label(data[,validation_predictor]),
                           partial_corr = round(readRDS("./results/descriptives/part_corr.rds"),
                                                          digits = 2),
                           cal_large = paste0(performance$point_estimates$cal_large,
                                              " (95% CI,",
                                              performance$confidence_intervals_lower$cal_large,
                                              "; ",
                                              performance$confidence_intervals_upper$cal_large,
                                              ")"),
                           cal_slope = paste0(performance$point_estimates$cal_slope,
                                              " (95% CI,",
                                              performance$confidence_intervals_lower$cal_slope,
                                              "; ",
                                              performance$confidence_intervals_upper$cal_slope,
                                              ")"),
                           delta_C_statistic = paste0(performance$point_estimates$c_stat,
                                                " (95% CI,",
                                                performance$confidence_intervals_lower$c_stat,
                                                "; ",
                                                performance$confidence_intervals_upper$c_stat,
                                                ")"),
                           delta_Scaled_Brier_score = paste0(performance$point_estimates$Brier_scaled,
                                                       " (95% CI,",
                                                       performance$confidence_intervals_lower$Brier_scaled,
                                                       "; ",
                                                       performance$confidence_intervals_upper$Brier_scaled,
                                                       ")"))
  return(result_row)
}











