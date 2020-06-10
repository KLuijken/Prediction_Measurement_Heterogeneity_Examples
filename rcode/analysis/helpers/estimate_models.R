#------------------------------------------------------------------------------#
# Predictor measurement heterogeneity empirical
# K Luijken
#
# Helper function to estimate ridge regression models and to obtain estimated 
# predictive performance measures
#------------------------------------------------------------------------------#

# Fit Ridge model
estimate_ridge <- function(data,derivation_predictor){
  covariates <- colnames(data)[!(colnames(data) %in% c("Y","X","W"))]
  matrix     <- as.matrix(cbind(data[derivation_predictor], data[,covariates]))
  
  # Tune lambda using cross-validation
  init       <- glmnet(matrix,data$Y, family="binomial", alpha = 0)
  sequence   <- exp(seq(from=min(init$lambda), 
                        by = log(init$lambda[2]) - log(init$lambda[1]),
                        length.out = 200))
  MRidgeCV   <- cv.glmnet(matrix,data$Y, 
                          nfolds=10,
                          type.measure="deviance",
                          family="binomial", alpha=0, lambda = sequence)
  
  # Estimate Ridge using optimal lambda
  ridge_model<- glmnet(matrix,data$Y,
                       lambda=MRidgeCV$lambda.min,
                       family="binomial", alpha=0)
  
  return(ridge_model)
}

# Either fit ML or Ridge
estimate_model <- function(data, method, derivation_predictor){
  covariates <- colnames(data)[!(colnames(data) %in% c("Y","X","W"))]
  
  model <- switch(method,
       ML = lrm(reformulate(termlabels = c(derivation_predictor,
                                           covariates),
                            response = "Y"), data = data),
       Ridge = estimate_ridge(data = data,
                              derivation_predictor = derivation_predictor))
}



# # Estimate linear predictor
# estimate_lp_ridge <- function(data, 
#                            model, 
#                            validation_predictor){
#   covariates <- colnames(data)[!(colnames(data) %in% c("Y","X","W"))]
#   lp         <- predict(model, 
#                         type = "link",
#                         as.matrix(cbind(data[validation_predictor],
#                                         data[,c(covariates)])),
#                         s = model$lambda[1])
#   expit_lp     <- expit(lp)
#   
#   return(expit_lp)
# }
# 
# # Estimate c-statistic
# c_stat_rigde <- function(lp, Y){
#   lp <- as.matrix(lp)
#   cats <- sort(unique(Y))
#   n_cat <- length(cats)
#   n0   <- sum(Y == cats[2])
#   n1   <- length(Y) - n0
#   r <- rank(lp[,1])
#   S0 <- sum(as.numeric(r[Y == cats[2]]))
#   (S0 - n0 * (n0 + 1)/2)/(as.numeric(n0) * as.numeric(n1))
# }
# 
# # Compute Brier score
# brier_ridge <- function(lp, data){
#   sum((data[,"y"]-lp)^2)/length(lp)
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
# 
# # Calibration
# calibration_ridge <- function(lp, Y){
#   calibration <- coef(glm(Y~lp, family = "binomial"))
#   
#   return(calibration)
# }

