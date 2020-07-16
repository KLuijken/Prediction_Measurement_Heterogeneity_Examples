#------------------------------------------------------------------------------#
# Predictor measurement heterogeneity empirical
# K Luijken
#
# Script to generate NHANES figure
#------------------------------------------------------------------------------#

# In our manuscript, we illustrated the concept of measurement heterogeneity using
# a simple prediction model that used bmi and bmi^2 to predict pre-diabetes.
# For this analysis, NHANES data was used.
# To reproduce the analysis, go to https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/default.aspx?BeginYear=2013
# and download the following files:
### BMX_H.XPT
### DIQ_H.XPT
### GLU_H.XPT
### WHQ_H.XPT
### DEMO_H.XPT
### OGTT_H.XPT

# For convenience, data were merged and cleaned in STATA 14. The do file code is
# shown below


# Data cleaning ----
#------------------------------------------------------------------------------#
# Remove all missings (complete case analysis)

#//// BMI self-report
#* Exclude missings/odd values
#keep if whd010 != 7777 & whd010 != 9999 
#keep if whd020 != 7777 & whd020 != 9999 

#* Transform variable units into meters and kilograms
# gen WHD010m = whd010 * 2.54 /100          
# gen WHD020kg = whd020 * 0.4535            
# gen WHDBMI = WHD020kg/(WHD010m * WHD010m) 
# gen whdbmiR = round(WHDBMI,0.1)

#//// Exclude patients diagnosed with diabetes
# gen byte diabetes = 1 if lbxglu >= 126 | diq010 == 1 | diq050 == 1 | diq070 == 1
# replace diabetes = 0 if lbxglu < 126 | diq010 ==2 | diq050 == 2 | diq070 == 2

# keep if diabetes == 0

#//// Generate outcome value 'prediabetes'
# gen prediabetes = 1 if lbxglu >= 100 & lbxglu < 126 & lbxglt >= 140 & lbxglt < 200
# replace prediabetes = 0 if lbxglu < 100 & lbxglt < 140

#//// Generate squared bmi values (part of the prediction model of interest)
# gen bmxbmi2 = bmxbmi * bmxbmi

# Generate figure ----
#------------------------------------------------------------------------------#

# Load libraries
library(haven)

# Load data
Ildata <- read_stata("./data/raw_data/prediab.dta")  # The datafile generated in STATA 14 as described above

# bmxbmi denotes the preferred measurement of bmi (measured during examination)
# WHDBMI denotes the pragmatic measurement of bmi (self-report)

# Compute bmi^2 for questionnaires
Ildata$WHDBMI <- round(Ildata$WHDBMI,1)
Ildata$WHDBMI2 <- Ildata$WHDBMI*Ildata$WHDBMI

# Listwise deletion missings
Ildata <- Ildata[complete.cases(Ildata$prediabetes),]
Ildata <- Ildata[complete.cases(Ildata$bmxbmi),]
Ildata <- Ildata[complete.cases(Ildata$WHDBMI),]

# Create lp bmi
fitX <- glm(Ildata$prediabetes~Ildata$bmxbmi + Ildata$bmxbmi2, family=binomial)
lpX <- fitX$coefficients[1] +fitX$coefficients[2] * Ildata$bmxbmi + fitX$coefficients[3]*Ildata$bmxbmi2
plot(density(lpX))

# Create lp bmi self-reported
fitW <- glm(Ildata$prediabetes~Ildata$WHDBMI + Ildata$WHDBMI2, family=binomial)
lpW <- fitW$coefficients[1] +fitW$coefficients[2] * Ildata$WHDBMI + fitW$coefficients[3]*Ildata$WHDBMI2
plot(density(lpW))

# Create combined plot
pdf("./results/illustration_nhanes/Fig1_densplot.pdf")
plot(density(lpX), 
     main = "", 
     xlim=c(-4,1),
     ylim=c(0,0.7),
     xlab = "Linear predictor", 
     ylab= "",
     yaxt="n",
     las=1,
     cex.axis =1.2,
     cex.lab =1.5,
     cex.main = 1.5)
lines(density(lpW), lty = 2)
legend("topright",legend=c("Preferred measurement BMI","Pragmatic measurement BMI"), lty=1:2, cex = 1, bty="n")
dev.off()