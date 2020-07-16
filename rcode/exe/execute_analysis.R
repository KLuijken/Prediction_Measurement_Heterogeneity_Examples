#------------------------------------------------------------------------------#
# Predictor measurement heterogeneity empirical
# K Luijken
#
# Execute script to process + analyze + visualize data
# Executing this analysis for 500 bootstrap repetitions takes around 2 hours
#------------------------------------------------------------------------------#

# Load librairies + source code ----
#------------------------------------------------------------------------------#
source(file = "./rcode/packages/packages.R")
source(file = "./rcode/analysis/standardize_data.R")
source(file = "./rcode/analysis/descriptives.R")
source(file = "./rcode/analysis/analyse_data.R")
source(file = "./rcode/visualisation/gen_tables_maintext.R")
source(file = "./rcode/visualisation/gen_graph_maintext.R")
source(file = "./rcode/visualisation/gen_graph_supp2.R")

# Load data  ----
#------------------------------------------------------------------------------#
# Read in data, for example:
original <- readRDS("./data/raw_data/simulated_data.rds")

# Remarks:
### Optially, use simulated data (Rcode ./data/raw_data/data_simulation.R)
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

# Set number of bootstrap repetitions
bootstrap_rep <- 500

# Store seeds
set.seed(57)
seeds <- sample(1:100000000, size= bootstrap_rep, replace=F)

# Vary estimatrion methods, predictor measurement homogeneity, predictor 
# measurement heterogeneity
use_analysis_scenarios <- expand.grid(method = c("ML", "Ridge"),
                                      derivation_predictor = c("X","W"),
                                      validation_predictor = c("X","W"))

# Store apparent performance in ./data/analysis/method/method_apparent_perf.rds
# Store internal performance in ./data/analysis/method/method_internal_perf.rds,
# including coordinates for calibrationslopes in ./data/analysis/calslope/
apply(use_analysis_scenarios,
      MARGIN = 1,
      FUN = analyse_data,
      data = data,
      bootstrap_rep = bootstrap_rep,
      seeds = seeds)


# Generate table (main text article)  ----
#------------------------------------------------------------------------------#

# Generate table measurement homogeneity
table_homogeneity <- rbind(
   generate_one_row_homogeneity(method = "ML",
                                derivation_predictor = "X",
                                validation_predictor = "X",
                                data = data),
   generate_one_row_homogeneity(method = "ML",
                                derivation_predictor = "W",
                                validation_predictor = "W",
                                data = data)
)
# Save output table
print(xtable(table_homogeneity),
      file = "./results/tables/measurement_homogeneity.txt",
      include.rownames = FALSE)


# Generate table measurement heterogeneity
table_heterogeneity <- rbind(
   generate_one_row_heterogeneity(method = "ML",
                                  derivation_predictor = "X",
                                  validation_predictor = "W",
                                  data = data),
   generate_one_row_heterogeneity(method = "ML",
                                  derivation_predictor = "W",
                                  validation_predictor = "X",
                                  data = data)
)

# Save output table
print(xtable(table_heterogeneity),
      file = "./results/tables/measurement_heterogeneity.txt",
      include.rownames = FALSE)

# Generate summary figure (main text article)  ----
#------------------------------------------------------------------------------#
# This function does not require arguments, but relies on the output generated 
# under "Analyse data" above
perf_plot_calLarge()

# Generate calibration plot (supplementary file)  ----
#------------------------------------------------------------------------------#
# Add plot with smooth calibration slopes, shown in supplementary file 2. This 
# example code shows the output for Maximum likelihood fit under predictor measurement
# heterogeneity (derivation measurement = X, validation measurement = W)
generate_spaghettiplot(perfmeasurex = readRDS("./data/analysis/calslope/ML_calslope_XW_xaxis.rds"),
                       perfmeasurey = readRDS("./data/analysis/calslope/ML_calslope_XW_yaxis.rds"),
                       B = bootstrap_rep)
