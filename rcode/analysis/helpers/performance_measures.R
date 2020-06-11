#------------------------------------------------------------------------------#
# Predictor measurement heterogeneity empirical
# K Luijken
#
# Helper functions to obtain estimated predictive performance measures
#------------------------------------------------------------------------------#

# Rescale to probabilities
expit <- function(x){1/(1+exp(-x))}

# Estimate linear predictor
estimate_lp <- function(data, 
                           model,
                           method,
                           validation_predictor){
  covariates <- colnames(data)[!(colnames(data) %in% c("Y","X","W"))]
  lp         <- switch(method,
                       as.character(ML) = predict(model, 
                                    as.matrix(cbind(data[validation_predictor],
                                                    data[,c(covariates)]))),
                       as.character(Ridge) = predict(model, 
                                       type = "link",
                                       as.matrix(cbind(data[validation_predictor],
                                                       data[,c(covariates)])),
                                       s = model$lambda[1]))
  
  return(lp)
}

# Compute scaled Brier score
scale_brier <- function(lp, Brierscore){
  expit_lp     <- expit(lp)
  BrierMax     <- mean(expit_lp) * (1-mean(expit_lp))
  BrierScaled  <- as.numeric((1-(Brierscore/ BrierMax)))
  
  return(BrierScaled)
}

# Estimate performance measures
estimate_performance <- function(lp, Y){
  pred  <- expit(lp)
  store <- val.prob(pred, Y)
  
  cal_large    <- as.numeric(store[["Intercept"]])
  cal_slope    <- as.numeric(store[["Slope"]])
  c_stat       <- as.numeric(store[["C (ROC)"]])
  Brier        <- as.numeric(store[["Brier"]])
  Brier_scaled <- scale_brier(pred, Brier)
  

  return(list(cal_large = cal_large,
           cal_slope = cal_slope,
           c_stat = c_stat,
           Brier = Brier,
           Brier_scaled = Brier_scaled))
}

# R squared
estimate_pseudoR2 <- function(lp, data){
  expit_lp   <- expit(lp)
  LL_fit     <- sum(data[,"Y"]*log(expit_lp)+(1-data[,"Y"])*log(1-expit_lp))
  LL_null    <- sum(data[,"Y"]*log(mean(data[,"Y"]))+(1-data[,"Y"])*log(1-mean(data[,"Y"])))
  cox        <- 1 - exp(-(LL_fit-LL_null)*2/nrow(data)) 
  cox_max    <- 1 - exp(2 * nrow(data) ^ (-1) * LL_null)
  nagelkerke <- cox/cox_max
  
  return(nagelkerke)
}


estimate_R2 <- function(data, model, method, lp){
  R2 <- switch(method,
                  as.character(ML) = model$stats[["R2"]],
                  as.character(Ridge) = estimate_pseudoR2(lp = lp,
                                            data = data))
}
