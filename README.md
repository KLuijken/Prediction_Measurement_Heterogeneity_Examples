# Prediction_Measurement_Heterogeneity_Examples

This code accompanies the paper "Changing predictor measurement procedures affected the performance of prediction models in clinical examples" (https://osf.io/xmdtr/). To reproduce the code, open the R-project and source all relevant code using the "Overall.R"-file.
<br>
<br>
<br>
<br>
<br>
<br>
To directly run the code that is provided here, the following simulated data could be used as data input in the "Overall.R"-file:

```{r}
require(MASS)
require(Matrix)
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
data    <- data.frame(mvrnorm(n, Mu, Sigma))
colnames(data) <- c("age","totChol","sysBP","BMI","BMIself")
P       <- plogis(-8.44 + 0.06*data$age + 0.001*data$totChol + 0.02*data$sysBP + 0.01*data$BMI)
data$TenYearCHD <- rbinom(n,1,P)
```
