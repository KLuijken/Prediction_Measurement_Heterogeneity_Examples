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


