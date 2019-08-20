######################################################################
# Predictor Measurement Heterogeneity (MH) Examples
# Author: K Luijken
# Example single analysis
#
# Date: July 2019
######################################################################

#------------------------------------------------------------#
# User instructions
#------------------------------------------------------------#

# This script contains code to quantify the effect of predictor
# measurement heterogeneity in a single predictor on the 
# predictive performance of a binary logistic regression 
# prediction model in empirical data. Output is stored as text or pdf files.


# Before running the script, load the data and specify the arguments
# under 'Standardize data' accordingly.


# Universal terminology
# Y refers to outcome variable
# X refers to predictor measurement with highest precision
# W refers to predictor measurement with lower precision
# Z refers to (vector of) other covariates in the model

#--------------------------------------------------------------------#
# Load packages and source functions
#--------------------------------------------------------------------#

# (Install and) Load packages:
require(rms)
require(glmnet)
require(xtable)
require(summarytools)

# Load functions
source("standdata.R")
source("bastab.R")
source("myvalprob.R")
source("perflog.R")


######################################################################
# Example 1
######################################################################

# Read in data, for example:
Original <- readRDS("MH_Fram_Data.rds")
### Make sure output variable is coded with 0/1 for no/yes event
### Make sure data does not contain missing values

# Specify the working directory to store output 
filepath <- "/Directory/as_desired/Output"

#--------------------------------------------------------------------#
# Standardize data  
#--------------------------------------------------------------------#

# Arguments:
## inputdata: the empirical dataset
## nameY: the name of the outcome variable, as character
## nameX: the name of the predictor of central interest in the analysis,
##        with the least measurement variance, as character
## nameW: the name of the second measurement of the predictor of central
##        interest in the analysis, with the most measurement variance, as character
## numberZ: the number of covariates in the prediction model, as integer number
## namesZ: a vector of names of the covariates in the prediction number,
##        as a vector of characters


Stand.data(inputdata=Original,
           nameY="TenYearCHD",
           nameX="BMI",
           nameW="BMIself",
           numberZ=6,
           namesZ=c("male","age","totChol","sysBP", "BPMeds", "currentSmoker"))


#--------------------------------------------------------------------#
# Create baseline table 
#--------------------------------------------------------------------#

# Arguments:
## Data: the dataset without missing values in
##       the standard format (output Stand.data function)
## Original: the original dataset, before imputation of missings and standardization

Bas.tab(Data = Data,
        Original = Original)


#--------------------------------------------------------------------#
# Perform analysis of interest 
#--------------------------------------------------------------------#

# Arguments:
## Data: the dataset in the standard format (output Stand.data function)
## B: the number of bootstrap resamplings

Perf.log(Data=Data,
         B = 500)



#--------------------------------------------------------------------#
# Create graphics
#--------------------------------------------------------------------#

source("spaggraph.R")

source("calpdf.R") 

# Note that the number of bootstrap resamplings is set to 500 in this file,
# so manual adjustment is needed in case of a different number of resamplings.
# In case of a low number of resamplings it may be desirable to reset the 
# alpha (increase it) in the spaggraph function



