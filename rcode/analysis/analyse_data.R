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
source("./rcode/analysis/helpers/internal_performance.R")

# Analyse data  ----
#------------------------------------------------------------------------------#

analyse_data <- function(data, bootstrap_rep){
  covariates <- colnames(data)[!(colnames(data) %in% c("Y","X","W"))]
  
  # Apparent predictive performance ----
  apparent_ML_XX <- apparent_performance(data = data,
                       method = "ML",
                       derivation_predictor = "X", 
                       validation_predictor = "X")
  apparent_ML_XW <- apparent_performance(data = data,
                       method = "ML",
                       derivation_predictor = "X", 
                       validation_predictor = "W")
  apparent_ML_WW <- apparent_performance(data = data,
                       method = "ML",
                       derivation_predictor = "W", 
                       validation_predictor = "W")
  apparent_ML_WX <- apparent_performance(data = data,
                       method = "ML",
                       derivation_predictor = "W", 
                       validation_predictor = "X")
  
  apparent_Ridge_XX <- apparent_performance(data = data,
                       method = "Ridge",
                       derivation_predictor = "X", 
                       validation_predictor = "X")
  apparent_Ridge_XW <- apparent_performance(data = data,
                       method = "Ridge",
                       derivation_predictor = "X", 
                       validation_predictor = "W")
  apparent_Ridge_WW <- apparent_performance(data = data,
                       method = "Ridge",
                       derivation_predictor = "W", 
                       validation_predictor = "W")
  apparent_Ridge_WX <- apparent_performance(data = data,
                       method = "Ridge",
                       derivation_predictor = "W", 
                       validation_predictor = "X")

  # Internal predictive performance (bootstrap) ----
  
  # Store seeds
  set.seed(57)
  seeds <- sample(1:100000000, size= bootstrap_rep, replace=F)
  
  # Bootstrap estimate of internal performance
  for(i in 1:bootstrap_rep) {
    set.seed(seeds[i])
    k         <- sample(1:nrow(data), 
                        size = nrow(data), 
                        replace = TRUE)
    bs_sample  <- data.frame(data[k,] )
    
    internal_performance(data = bs_sample,
      method = "ML",
      derivation_predictor = "X", 
      validation_predictor = "X",
      results_apparent = apparent_ML_XX,
      seed = seeds[i])
    internal_performance(data = bs_sample,
      method = "ML",
      derivation_predictor = "X", 
      validation_predictor = "W",
      results_apparent = apparent_ML_XW,
      seed = seeds[i])
    internal_performance(data = bs_sample,
      method = "ML",
      derivation_predictor = "W", 
      validation_predictor = "W",
      results_apparent = apparent_ML_WW,
      seed = seeds[i])
    internal_performance(data = bs_sample,
      method = "ML",
      derivation_predictor = "W", 
      validation_predictor = "X",
      results_apparent = apparent_ML_WX,
      seed = seeds[i])
    
    internal_performance(data = bs_sample,
      method = "Ridge",
      derivation_predictor = "X", 
      validation_predictor = "X",
      results_apparent = apparent_Ridge_XX,
      seed = seeds[i])
    internal_performance(data = bs_sample,
      method = "Ridge",
      derivation_predictor = "X", 
      validation_predictor = "W",
      results_apparent = apparent_Ridge_XW,
      seed = seeds[i])
    internal_performance(data = bs_sample,
      method = "Ridge",
      derivation_predictor = "W", 
      validation_predictor = "W",
      results_apparent = apparent_Ridge_WW,
      seed = seeds[i])
    internal_performance(data = bs_sample,
      method = "Ridge",
      derivation_predictor = "W", 
      validation_predictor = "X",
      results_apparent = apparent_Ridge_WX,
      seed = seeds[i])
    
  }
  
}
