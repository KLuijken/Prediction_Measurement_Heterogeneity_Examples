#------------------------------------------------------------------------------#
# Predictor measurement heterogeneity empirical
# K Luijken
#
# Helper functions to estimate logistic regression and ridge regression models 
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