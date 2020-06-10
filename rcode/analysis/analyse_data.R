#------------------------------------------------------------------------------#
# Predictor measurement heterogeneity empirical
# K Luijken
#
# Analyse data: estimate models and validate under predictor measurement 
# homogeneity and heterogeneity
#------------------------------------------------------------------------------#


# Load libraries  ----
#------------------------------------------------------------------------------#
source("./rcode/analysis/helpers/adjusted_valprob.R")
source("./rcode/analysis/helpers/estimate_models.R")
source("./rcode/analysis/helpers/performance_measures.R")
source("./rcode/analysis/helpers/apparent_performance.R")

# Analyse data  ----
#------------------------------------------------------------------------------#

analyse_data <- function(data, bootstrap_rep){
  covariates <- colnames(data)[!(colnames(data) %in% c("Y","X","W"))]
  
  # Apparent predictive performance 
  # forms <- expand.grid(method = c("ML", "Ridge"), predictor = c("X","W"))
  # apply(forms,
  #       MARGIN = 1,
  #       FUN = apparent_performance,
  #       data = data,
  #       method = forms[['method']],
  #       derivation_predictor = forms[['predictor']],
  #       validation_predictor = forms[['predictor']])
  
  apparent_performance(data = data,
                       method = "ML",
                       derivation_predictor = "X", 
                       validation_predictor = "X")
  apparent_performance(data = data,
                       method = "ML",
                       derivation_predictor = "W", 
                       validation_predictor = "W")
  apparent_performance(data = data,
                       method = "Ridge",
                       derivation_predictor = "X", 
                       validation_predictor = "X")
  apparent_performance(data = data,
                       method = "Ridge",
                       derivation_predictor = "W", 
                       validation_predictor = "W")
  
  
  
  # Internal predictive performance (bootstrap)
  
  
  
}
