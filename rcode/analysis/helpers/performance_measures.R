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
                       ML = predict(model, 
                                    as.matrix(cbind(data[validation_predictor],
                                                    data[,c(covariates)]))),
                       Ridge = predict(model, 
                                       type = "link",
                                       as.matrix(cbind(data[validation_predictor],
                                                       data[,c(covariates)])),
                                       s = model$lambda[1]))
  
  return(lp)
}

# Estimate c-statistic
estimate_cstat <- function(lp, Y){
  expit_lp  <- expit(lp)
  expit_lp <- as.matrix(expit_lp)
  cats <- sort(unique(Y))
  n_cat <- length(cats)
  n0   <- sum(Y == cats[2])
  n1   <- length(Y) - n0
  r <- rank(expit_lp[,1])
  S0 <- sum(as.numeric(r[Y == cats[2]]))
  (S0 - n0 * (n0 + 1)/2)/(as.numeric(n0) * as.numeric(n1))
}

# Compute Brier score
compute_brier <- function(lp, Y){
  expit_lp <- expit(lp)
  sum((Y-expit_lp)^2)/length(expit_lp)
}

# Compute scaled Brier score
scale_brier <- function(lp, Brierscore){
  expit_lp     <- expit(lp)
  BrierMax     <- mean(expit_lp) * (1-mean(expit_lp))
  BrierScaled  <- as.numeric((1-(Brierscore/ BrierMax)))
  
  return(BrierScaled)
}

# Calibration
estimate_calibrationLarge <- function(lp, Y){
  calibration <- glm(Y~offset(lp), family = "binomial")
  calLarge    <- calibration$coefficients[1]
  
  return(calLarge)
}

estimate_calibrationslope <- function(lp, Y){
  calibration <- glm(Y~lp, family = "binomial")
  calslope    <- calibration$coefficients[2]
  
  return(calslope)
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
                  ML = model$stats[["R2"]],
                  Ridge = estimate_pseudoR2(lp = lp,
                                            data = data))
}



