#------------------------------------------------------------------------------#
# Predictor measurement heterogeneity empirical
# K Luijken
#
# Helper function to estimate ridge regression models
#------------------------------------------------------------------------------#

estimate_ridge <- function(data,predictor_measurement){
  covariates <- colnames(data)[!(colnames(data) %in% c("Y","X","W"))]
  matrix     <- as.matrix(cbind(data[predictor_measurement], data[,covariates]))
  
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