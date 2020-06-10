#------------------------------------------------------------------------------#
# Predictor measurement heterogeneity empirical
# K Luijken
#
# Helper functions to obtain estimated predictive performance measures
#------------------------------------------------------------------------------#

# # Rescale to probabilities
# expit <- function(x){1/(1+exp(-x))}
# 
# # Estimate linear predictor
# estimate_lp_ML <- function(data, 
#                         model, 
#                         validation_predictor){
#   covariates <- colnames(data)[!(colnames(data) %in% c("Y","X","W"))]
#   lp         <- predict(model, 
#                         as.matrix(cbind(data[validation_predictor],
#                                         data[,c(covariates)])))
#   expit_lp     <- expit(lp)
#   
#   return(expit_lp)
# }
# 
# # Compute scaled Brier score
# scale_brier <- function(lp, Brierscore){
#   pred         <- plogis(lp)
#   BrierMax     <- mean(pred) * (1-mean(pred))
#   BrierScaled  <- as.numeric((1-(Brierscore/ BrierMax)))
#   
#   return(BrierScaled)
# }

# Workflow helper ----
#------------------------------------------------------------------------------#
# Helper function to obtain apparent predictive performance measures
apparent_performance <- function(data,
                                 method,
                                 derivation_predictor, 
                                 validation_predictor){
  # Estimate model (switch Maximum Likelihood versus Ridge)
  model    <- estimate_model(data = data,
                             method = method,
                             derivation_predictor = derivation_predictor)
  
  # Estimate apparent performance
  lp <- estimate_lp(data = data,
                          model = model,
                          method =  method,
                          validation_predictor = validation_predictor)
  apparent <- data.frame(calLarge = estimate_calibrationLarge(lp = lp,
                                                         Y = data[,"Y"]),
                         calslope = estimate_calibrationslope(lp = lp,
                                                         Y = data[,"Y"]),
                         cstat = estimate_cstat(lp = lp,
                                                Y = data[,"Y"]),
                         Brier = compute_brier(lp = lp,
                                               Y = data[,"Y"]),
                         Brier_scaled = scale_brier(lp = lp,
                                                    Brierscore = compute_brier(lp = lp,
                                                                               Y = data[,"Y"])),
                         R2 = estimate_R2(data =  data,
                                          model =  model,
                                          method = method,
                                          lp = lp))
  # Save measures and output
  saveRDS(apparent, file = paste0("./results/analysis/",method,"_apparent_perf_",
                                  derivation_predictor,".rds"))
  
  pdf(paste("./results/analysis/",method,"_apparent_perf_",
            derivation_predictor,".pdf",sep = ""))
  adjusted_val.prob(lp,data$Y)
  dev.off()
  
  
}

