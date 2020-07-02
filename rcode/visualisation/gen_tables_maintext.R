#------------------------------------------------------------------------------#
# Predictor measurement heterogeneity empirical
# K Luijken
#
# Generate tables
#------------------------------------------------------------------------------#

# Source code ----
#------------------------------------------------------------------------------#
source("./rcode/visualisation/helper/process_analysis_results.R")

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











