#------------------------------------------------------------------------------#
# Predictor measurement heterogeneity empirical
# K Luijken
#
# Generate tables
#------------------------------------------------------------------------------#

# Helper ----
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
  
  return(list(point_estimates, confidence_intervals_lower, confidence_intervals_upper))
}

process_results_homogeneity(method = "ML",
                            derivation_predictor = "X",
                            validation_predictor = "X",
                            data = data)


# Generate table measurement homogeneity  ----
#------------------------------------------------------------------------------#

generate_one_row <- function(method, derivation_predictor, validation_predictor, data){
  performance <- paste0("./results/analysis/",method,
                        "/internal_perf_",
                        derivation_predictor,
                        validation_predictor,".rds")
  
  result_row <- data.frame(Event_fraction = nrow(data[data$Y==1,])/nrow(data),
                           Measurement_strategy = Hmisc::label(data$X),
                           Mean_value_measurement = round(mean(data$X),
                                                          digits = 2),
                           Standard_deviation_measurement = round(
                             sd(data$X), digits = 2),
                           C_statistic = round(performance$c_stat, digits = 2),
                           Scaled_Brier_score = round(performance$Brier_scaled,
                                                      digits = 2))
  result_row <- round(result_row, digits = 2)
  
  return(result_row)
}



generate_one_row(method = "ML",
                 derivation_predictor = "X",
                 data = data)

# Load results
apparent <- do.call('rbind', lapply(
  list("./results/analysis/ML_apparent_perf_X.rds",
       "./results/analysis/ML_apparent_perf_W.rds"), readRDS))







# Apparent
Standard deviation measurement	

# Internal
Scenario	Measurement strategy at derivation	Measurement strategy at validation	ρpart	Calibration-in-the-large	Calibration slope	ΔC-statistic a100	Δ scaled brier score

"./results/descriptives/part_corr.rds"
