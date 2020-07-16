#------------------------------------------------------------------------------#
# Predictor measurement heterogeneity empirical
# K Luijken
#
# Function to standardize the input data frame
#------------------------------------------------------------------------------#

standardize_data <-function(inputdata,nameY,nameX,nameW,numberZ, namesZ){
  
  # Check whether argument input is provided correctly
  cats <- unique(inputdata[(colnames(inputdata)==nameY)])
  if(identical(all.equal(cats[,1], c(0,1)), FALSE)) stop("Make sure that the outcome variable is coded as '0' for nonevents and '1' for events.")
  if(is.character(nameY)==F) stop("Argument 'nameY' should specify the name of the outcome variable")
  if(is.character(nameX)==F) stop("Argument 'nameX' should specify the name of the predictor of interest, measured as X")
  if(is.character(nameW)==F) stop("Argument 'nameW' should specify the name of the predictor of interest, measured as W")
  if(length(namesZ) != numberZ) stop("The number of covariates ('numberZ') must be equal to the number of names of covariates ('namesZ')")
  if(is.character(namesZ)==F) stop("Argument 'namesZ' has to be a vector of variable names of the relevant covariates")
  
  # Create dataframe with standardized names (Y for outcome, X for preferred
  # predictor measurement, W for pragmatic predictor measurement)
  data <- data.frame(inputdata[,c(nameY,nameX,nameW,namesZ)])
  colnames(data) <- c("Y","X","W",paste("Z",1:length(namesZ),sep=""))
  for(j in 1:ncol(data))  Hmisc::label(data[,j]) <- c(nameY,nameX,nameW,namesZ)[j]
  
  return(data)
}

