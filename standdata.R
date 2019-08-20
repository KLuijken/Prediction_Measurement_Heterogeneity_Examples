
### Function to load data in standardized format


Stand.data <-function(inputdata,nameY,nameX,nameW,numberZ, namesZ){
  
  cats <- unique(inputdata[(colnames(inputdata)==nameY)])
  if(identical(all.equal(cats[,1], c(0,1)), FALSE)) stop("Make sure that the outcome variable is coded as '0' for nonevents and '1' for events.")
  if(is.character(nameY)==F) stop("Argument 'nameY' should specify the name of the outcome variable")
  if(is.character(nameX)==F) stop("Argument 'nameX' should specify the name of the predictor of interest, measured as X")
  if(is.character(nameW)==F) stop("Argument 'nameW' should specify the name of the predictor of interest, measured as W")
  if(length(namesZ) != numberZ) stop("The number of covariates ('numberZ') must be equal to the number of names of covariates ('namesZ')")
  if(is.character(namesZ)==F) stop("Argument 'namesZ' has to be a vector of variable names of the relevant covariates")
  
  Data <- data.frame(inputdata[,c(nameY,nameX,nameW,namesZ)])
  colnames(Data) <- c("Y","X","W",paste("Z",1:length(namesZ),sep=""))
  for(j in 1:ncol(Data))  Hmisc::label(Data[,j]) <- c(nameY,nameX,nameW,namesZ)[j]
  
  Data <<- Data
  z <<- numberZ
  covariates <<- colnames(Data[,4:(4+z-1)])
}


