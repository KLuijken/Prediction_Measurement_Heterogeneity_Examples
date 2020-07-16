#------------------------------------------------------------------------------#
# Predictor measurement heterogeneity empirical
# K Luijken
#
# Script to provide data descriptives
#------------------------------------------------------------------------------#

# Some notes on the format of the output
#------------------------------------------------------------------------------#
# Output files are saved in ./results/descriptives.
# Descriptives of each variable (mean/sd/minimum/median/maximum/missing values)
#     will be presented as a table in an .html file, in the style of the dfSummary
#     function in the summarytools package.
# The correlation matrix is saved as an .rds file (corr_matrix.rds).
# The partial correlation are stored in a dataframe and saved as an .rds file 
#     (partcorr.rds).
# The similarity of the preferred (X) and pragmatic (W) measurement is plotted
#     on a scatterplot and saved as .pdf file (similarityXW.pdf).


# Helpers ----
#------------------------------------------------------------------------------#
# Helper function to generate dfSummary .html files
create_dfsummary <- function(standardized, original){
  print(dfSummary(original,round.digits=3),
        file=file.path("./results/descriptives/originaldata_descriptives.html"))
  print(dfSummary(standardized,round.digits=3),
        file=file.path("./results/descriptives/standardizeddata_descriptives.html"))
  print(dfSummary(standardized[standardized[,"Y"]==0,],round.digits=3),
        file=file.path("./results/descriptives/standardizeddata_descriptives.html"),
        append=T)
  print(dfSummary(standardized[standardized[,"Y"]==1,],round.digits=3),
        file=file.path("./results/descriptives/standardizeddata_descriptives.html"),
        append=T)
}

# Helper function to save .rds file with correlation matrix
create_corrmatrix <- function(standardized){
  correlation <- cor(standardized,method= "pearson")
  saveRDS(correlation, file = "./results/descriptives/corr_matrix.rds")
}

# Helper function to save .rds file with partial correlation and outcome frequency
create_partcorr <- function(standardized){
  # Saven covariate names in standardized way
  covariates<- colnames(standardized)[!(colnames(standardized) %in% c("Y","X","W"))]
  
  # Obtain residuals for X and W given Y and covariates
  mm1       <- lm(reformulate(termlabels = c('Y',covariates), response = 'X'),
                  data=standardized)
  res1      <- mm1$residuals
  mm2       <- lm(reformulate(termlabels = c('Y',covariates), response = 'W'),
                  data=standardized)
  res2      <- mm2$residuals
  
  # Partical correlation
  part_corr <- data.frame(part_corr = cor(res1,res2))
  saveRDS(part_corr, file = "./results/descriptives/part_corr.rds")
}

# Helper function to generate visualiation of X-W similarity
create_scatterXW <- function(standardized){
  pdf("./results/descriptives/plotXW.pdf", width = 4, height = 4)
  plot(standardized$X,
       standardized$W, 
       ylab = paste(label(standardized$X)),
       xlab = paste(label(standardized$W)),
       las=1,
       cex.lab=0.75,
       cex.axis=0.75)
  abline(lm(standardized$W~standardized$X),col="black")
  dev.off()
}

# Workhorse descriptive analysis ----
#------------------------------------------------------------------------------#
analyze_descriptives <- function(standardized, original){
  create_dfsummary(standardized =  standardized,
                   original = original)
  create_corrmatrix(standardized = standardized)
  create_partcorr(standardized = standardized)
  create_scatterXW(standardized = standardized)
}
