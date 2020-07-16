#------------------------------------------------------------------------------#
# Predictor measurement heterogeneity empirical
# K Luijken
#
# Script to generate a simulated dataset to run the code
#------------------------------------------------------------------------------#

# Since the empirical data used in the article is not publicly available,
# we provide a simulated dataset to run the analyses.

# Load libraries ----
#------------------------------------------------------------------------------#
require(MASS)
require(Matrix)


# Simulate data ----
#------------------------------------------------------------------------------#

# Parameters (sample size, mean values, variance-covariance matrix)
n       <- 3000
Mu      <- c(60,240,130,26,24)
Sigma   <- matrix(c(73,101,73,5,4,
                    101,1944,214,22,20, 
                    73,214,488,30,26,
                    5,22,30,17,15,
                    4,20,26,15,16),
                  nrow=5,
                  ncol=5)
Sigma   <- nearPD(Sigma)$mat

# Simulate data
set.seed(57)
data    <- data.frame(mvrnorm(n, Mu, Sigma))
colnames(data) <- c("age","totChol","sysBP","BMI","BMIself")
P       <- plogis(-8.44 + 0.06*data$age + 0.001*data$totChol + 0.02*data$sysBP + 0.01*data$BMI)
data$TenYearCHD <- rbinom(n,1,P)

# Save dataframe
saveRDS(data,file = "./data/raw_data/simulated_data.rds")
