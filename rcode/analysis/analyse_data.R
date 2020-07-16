#------------------------------------------------------------------------------#
# Predictor measurement heterogeneity empirical
# K Luijken
#
# Analyse data: estimate models and validate under predictor measurement 
# homogeneity and heterogeneity
#------------------------------------------------------------------------------#


# Load libraries  ----
#------------------------------------------------------------------------------#
source("./rcode/analysis/helpers/estimate_models.R")
source("./rcode/analysis/helpers/performance_measures.R")
source("./rcode/analysis/helpers/workflow_apparent_performance.R")
source("./rcode/analysis/helpers/workflow_internal_performance.R")

# Analyse data  ----
#------------------------------------------------------------------------------#

analyse_data <- function(analysis_scenarios, data, bootstrap_rep, seeds){
  covariates <- colnames(data)[!(colnames(data) %in% c("Y","X","W"))]
  
  # Apparent predictive performance ----
  apparent_perf <- apparent_performance(data = data,
                       method = analysis_scenarios[['method']],
                       derivation_predictor = analysis_scenarios[['derivation_predictor']], 
                       validation_predictor = analysis_scenarios[['validation_predictor']])
  
  # Internal predictive performance (bootstrap) ----
  
  for(i in 1:bootstrap_rep) {
    set.seed(seeds[i])
    k         <- sample(1:nrow(data), 
                        size = nrow(data), 
                        replace = TRUE)
    bs_sample  <- data.frame(data[k,] )
    internal_performance(data = bs_sample,
      method = analysis_scenarios[['method']],
      derivation_predictor = analysis_scenarios[['derivation_predictor']],
      validation_predictor = analysis_scenarios[['validation_predictor']],
      results_apparent = apparent_perf,
      seed = seeds[i])
  }
  
}
