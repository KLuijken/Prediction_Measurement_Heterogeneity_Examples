#------------------------------------------------------------------------------#
# Predictor measurement heterogeneity empirical
# K Luijken
#
# Execute script to process + analyze + visualize data
#------------------------------------------------------------------------------#


# Load librairies + source code ----
#------------------------------------------------------------------------------#
source(file = "./rcode/packages/packages.R")
source(file = "./rcode/analysis/standardize_data.R")
source(file = "./rcode/analysis/descriptives.R")

# Load data  ----
#------------------------------------------------------------------------------#
# Read in data, for example:
original <- readRDS("MH_Fram_Data.rds")

# Remarks:
### Optially, use simulated data (Rcode here)
### Make sure output variable is coded with 0/1 for no/yes event
### Make sure data does not contain missing values

# Standardize data  ----
#------------------------------------------------------------------------------#
data <- standardize_data(inputdata = original,
                         nameY = "TenYearCHD",
                         nameX = "BMI",
                         nameW = "BMIself",
                         numberZ = 3,
                         namesZ=c("age","totChol","sysBP"))

# Generate descriptives  ----
#------------------------------------------------------------------------------#

analyze_descriptives(standardized = data,
                     original = original)



# Analyse data: apparent and internal predictive performance  ----
#------------------------------------------------------------------------------#

# Store seeds
set.seed(57)
seeds <- sample(1:100000000, size= bootstrap_rep, replace=F)

# Vary estimatrion methods, predictor measurement homogeneity, predictor 
# measurement heterogeneity
use_analysis_scenarios <- expand.grid(method = c("ML", "Ridge"),
                                      derivation_predictor = c("X","W"),
                                      validation_predictor = c("X","W"))

# Store apparent performance in .results/analysis/method/method_apparent_perf.rds
# Store internal performance in .results/analysis/method/method_internal_perf.rds,
# including coordinates for calibrationslopes in .results/analysis/calslope/

apply(use_analysis_scenarios,
      MARGIN = 1,
      FUN = analyse_data,
      data = data,
      bootstrap_rep = 5,
      seeds = seeds)