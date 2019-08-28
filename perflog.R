Ridge <- function(data,meas){
  matrix <- as.matrix(cbind(data[meas],data[,4:(3+z)]))
  init <- glmnet(matrix,data$Y, family="binomial", alpha = 0)
  sequence <- exp(seq(from=min(init$lambda), by = log(init$lambda[2])-log(init$lambda[1]), length.out = 200))
  MRidgeCV <- cv.glmnet(matrix,data$Y,nfolds=10,type.measure="deviance", family="binomial", alpha=0, lambda = sequence)
  assign(paste("Mridge",meas, sep=""), glmnet(matrix,data$Y,lambda=MRidgeCV$lambda.min, family="binomial", alpha=0))
  
  return(get(paste("Mridge",meas, sep="")))
}

LP <- function(data,mod,der,val){
  lp <- predict(get(paste0("bs.M",der,mod),envir=parent.frame()),
                as.matrix(cbind(data[val], data[,4:(3+z)])))
}

Validation <- function(lpinput,datay){
  pred <- plogis(lpinput)
  store <- val.prob(pred,datay)

  CalL         <- as.numeric(store[["Intercept"]])
  Calsl        <- as.numeric(store[["Slope"]])
  Cstat        <- as.numeric(store[["C (ROC)"]])
  Brier        <- as.numeric(store[["Brier"]])
  BrierMax     <- mean(pred) * (1-mean(pred))
  BrierScaled  <- as.numeric((1-(Brier/ BrierMax)))
  R2           <- as.numeric(store[[3]])

  return(c(CalL=CalL, Calsl=Calsl,Cstat=Cstat,Brier=Brier, BrierScaled=BrierScaled, R2=R2))
}

Optcor <- function(perfmeas, optmeas, der,val){
  tab <- matrix(0,nrow=6,ncol=3)
  rownames(tab) <- c("CalL", "Calslope", "C.stat", "Brier","BrierScaled","R2")
  colnames(tab) <- c("average", "cilow","ciup")
  tab["CalL","average"] <- median(perfmeas[paste0(der,val),,"CalL"])
  tab["Calslope","average"] <- median(perfmeas[paste0(der,val),,"Calslope"])
  tab["C.stat","average"] <- get(paste0("M",der,".lrm"), envir = parent.frame())$stats['C'] -  mean(optmeas[paste0(der,val),,"C.stat"])
  tab["Brier","average"] <- get(paste0("M",der,".lrm"), envir = parent.frame())$stats['Brier'] -  mean(optmeas[paste0(der,val),,"Brier"])
  tab["BrierScaled","average"] <- get(paste0("BrierScaled.",der), envir = parent.frame()) -  mean(optmeas[paste0(der,val),,"BrierScaled"])
  tab["R2","average"] <- get(paste0("M",der,".lrm"), envir = parent.frame())$stats['R2'] -  mean(optmeas[paste0(der,val),,"R2"])

  tab["CalL","cilow"] <- quantile(perfmeas[paste0(der,val),,"CalL"], 0.025, names=F)
  tab["Calslope","cilow"] <- quantile(perfmeas[paste0(der,val),,"Calslope"], 0.025, names=F)
  tab["C.stat","cilow"] <- get(paste0("ciCstat.",der), envir = parent.frame())[1] -  mean(optmeas[paste0(der,val),,"C.stat"])
  tab["Brier","cilow"] <- get(paste0("ciBrier.",der), envir = parent.frame())[1] -  mean(optmeas[paste0(der,val),,"Brier"])
  tab["BrierScaled","cilow"] <- get(paste0("ciBrierScaled.",der), envir = parent.frame())[1] -  mean(optmeas[paste0(der,val),,"BrierScaled"])
  tab["R2","cilow"] <- get(paste0("ciRsq.",der), envir = parent.frame())[1] -  mean(optmeas[paste0(der,val),,"R2"])

  tab["CalL","ciup"] <- quantile(perfmeas[paste0(der,val),,"CalL"], 0.975, names=F)
  tab["Calslope","ciup"] <- quantile(perfmeas[paste0(der,val),,"Calslope"], 0.975, names=F)
  tab["C.stat","ciup"] <- get(paste0("ciCstat.",der), envir = parent.frame())[2] -  mean(optmeas[paste0(der,val),,"C.stat"])
  tab["Brier","ciup"] <- get(paste0("ciBrier.",der), envir = parent.frame())[2] -  mean(optmeas[paste0(der,val),,"Brier"])
  tab["BrierScaled","ciup"] <- get(paste0("ciBrierScaled.",der), envir = parent.frame())[2] -  mean(optmeas[paste0(der,val),,"BrierScaled"])
  tab["R2","ciup"] <- get(paste0("ciRsq.",der), envir = parent.frame())[2] -  mean(optmeas[paste0(der,val),,"R2"])

  return(tab)
}

Opti <- function(optmeas,der,val){
  meanopt <- matrix(0,nrow=4,ncol=3)
  rownames(meanopt) <- c("C.stat", "Brier","BrierScaled","R2")
  colnames(meanopt) <- c("average","CIlow", "CIup")
  meanopt["C.stat",] <- c(mean(optmeas[paste0(der,val),,"C.stat"]), quantile(optmeas[paste0(der,val),,"C.stat"], c(0.025,0.975), names=F))
  meanopt["Brier",] <- c(mean(optmeas[paste0(der,val),,"Brier"]), quantile(optmeas[paste0(der,val),,"Brier"], c(0.025,0.975), names=F))
  meanopt["BrierScaled",] <- c(mean(optmeas[paste0(der,val),,"BrierScaled"]), quantile(optmeas[paste0(der,val),,"BrierScaled"], c(0.025,0.975), names=F))
  meanopt["R2",] <- c(mean(optmeas[paste0(der,val),,"R2"]), quantile(optmeas[paste0(der,val),,"R2"], c(0.025,0.975), names=F))
  name <- paste0("optimism_",der,val)
  write.table(meanopt, file = file.path(filepath,paste0(name,".txt")), row.names = T)
}



Perf.log <- function(Data,B){
  
  # Estimate models
  xmod <- reformulate(termlabels = c('X', covariates), response = 'Y')
  MX.lrm <- lrm(xmod,data=Data)
  MX.glm <- glm(xmod,data=Data,family="binomial")
  
  wmod <- reformulate(termlabels = c('W', covariates), response = 'Y')
  MW.lrm <- lrm(wmod,data=Data)
  MW.glm <- glm(wmod,data=Data,family="binomial")
  
  # Estimate Ridge regression models
  MX.ridge <- Ridge(Data,"X")
  MW.ridge <- Ridge(Data,"W")

  # Store preformance measures
  statsDer <- list(MX.lrm$stats,MW.lrm$stats)
  names(statsDer) <- c("statsMX","statsMW")
  saveRDS(statsDer, file= paste(filepath,"/statsDer.rds", sep=""))
  
  # Store calibration plots
  lpX <- predict(MX.lrm,as.matrix(cbind(Data$X, Data[,4:(3+z)])))
  pred.DerX <- plogis(lpX)
  
  pdf(paste(filepath,"/DerivationX.pdf",sep = ""))
  my.val.prob(pred.DerX,Data$Y,statloc=F)
  dev.off()
  pdf(paste(filepath,"/DerivationXlegend.pdf",sep = ""))
  my.val.prob(pred.DerX,Data$Y)
  dev.off()
  
  lpW <- predict(MW.lrm,as.matrix(cbind(Data$W, Data[,4:(3+z)])))
  pred.DerW <- plogis(lpW)
  
  pdf(paste(filepath,"/DerivationW.pdf",sep = ""))
  my.val.prob(pred.DerW,Data$Y,statloc=F)
  dev.off()
  pdf(paste(filepath,"/DerivationWlegend.pdf",sep = ""))
  my.val.prob(pred.DerW,Data$Y)
  dev.off()
  
  # Create bootstrap storage
  Perf.bs.homog <- array(dim=c(2,nrow=B,ncol=6))
  dimnames(Perf.bs.homog) <- list(c("XX", "WW"),NULL,c("CalL", "Calslope", "C.stat", "Brier","BrierScaled","R2"))
  Perf.D.homog <- array(dim=c(2,nrow=B,ncol=6))
  dimnames(Perf.D.homog) <- list(c("XX", "WW"),NULL,c("CalL", "Calslope", "C.stat", "Brier","BrierScaled","R2"))
  Optimism.homog <- array(dim=c(2,nrow=B,ncol=6))
  dimnames(Optimism.homog) <- list(c("XX", "WW"),NULL,c("CalL", "Calslope", "C.stat", "Brier","BrierScaled","R2"))
  
  Perf.bs.heterog <- array(dim=c(2,nrow=B,ncol=6))
  dimnames(Perf.bs.heterog) <- list(c("XW", "WX"),NULL,c("CalL", "Calslope", "C.stat", "Brier","BrierScaled","R2"))
  Perf.D.heterog <- array(dim=c(2,nrow=B,ncol=6))
  dimnames(Perf.D.heterog) <- list(c("XW", "WX"),NULL,c("CalL", "Calslope", "C.stat", "Brier","BrierScaled","R2"))
  Optimism.heterog <- array(dim=c(2,nrow=B,ncol=6))
  dimnames(Optimism.heterog) <- list(c("XW", "WX"),NULL,c("CalL", "Calslope", "C.stat", "Brier","BrierScaled","R2"))
 
  Delta <- matrix(0,nrow=B, ncol =12)
  colnames(Delta) <- c("DeltaCalLXW","DeltaCalLWX","DeltaCalslopeXW","DeltaCalslopeWX","DeltaCXW","DeltaCWX","DeltaBrierXW","DeltaBrierWX", "DeltaBrierscaledXW","DeltaBrierscaledWX", "DeltaR2XW","DeltaR2WX")
  
  Perf.bs.homog.ridge <- array(dim=c(2,nrow=B,ncol=6))
  dimnames(Perf.bs.homog.ridge) <- list(c("XX", "WW"),NULL,c("CalL", "Calslope", "C.stat", "Brier","BrierScaled","R2"))
  Perf.D.homog.ridge <- array(dim=c(2,nrow=B,ncol=6))
  dimnames(Perf.D.homog.ridge) <- list(c("XX", "WW"),NULL,c("CalL", "Calslope", "C.stat", "Brier","BrierScaled","R2"))
  Optimism.homog.ridge <- array(dim=c(2,nrow=B,ncol=6))
  dimnames(Optimism.homog.ridge) <- list(c("XX", "WW"),NULL,c("CalL", "Calslope", "C.stat", "Brier","BrierScaled","R2"))
  
  Perf.bs.heterog.ridge <- array(dim=c(2,nrow=B,ncol=6))
  dimnames(Perf.bs.heterog.ridge) <- list(c("XW", "WX"),NULL,c("CalL", "Calslope", "C.stat", "Brier","BrierScaled","R2"))
  Perf.D.heterog.ridge <- array(dim=c(2,nrow=B,ncol=6))
  dimnames(Perf.D.heterog.ridge) <- list(c("XW", "WX"),NULL,c("CalL", "Calslope", "C.stat", "Brier","BrierScaled","R2"))
  Optimism.heterog.ridge <- array(dim=c(2,nrow=B,ncol=6))
  dimnames(Optimism.heterog.ridge) <- list(c("XW", "WX"),NULL,c("CalL", "Calslope", "C.stat", "Brier","BrierScaled","R2"))
  
  Delta.ridge <- matrix(0,nrow=B, ncol =12)
  colnames(Delta.ridge) <- c("DeltaCalLXW","DeltaCalLWX","DeltaCalslopeXW","DeltaCalslopeWX","DeltaCXW","DeltaCWX","DeltaBrierXW","DeltaBrierWX", "DeltaBrierscaledXW","DeltaBrierscaledWX", "DeltaR2XW","DeltaR2WX")
  
  
  cal.XX  <- array(dim=c(B,nrow=length(c(seq(from=1,to=nrow(Data),by=round(nrow(Data)/20)),nrow(Data))),ncol=2))
  cal.XX.shrink  <- array(dim=c(B,nrow=length(c(seq(from=1,to=nrow(Data),by=round(nrow(Data)/20)),nrow(Data))),ncol=2))
  cal.XW  <- array(dim=c(B,nrow=length(c(seq(from=1,to=nrow(Data),by=round(nrow(Data)/20)),nrow(Data))),ncol=2))
  cal.XW.shrink  <- array(dim=c(B,nrow=length(c(seq(from=1,to=nrow(Data),by=round(nrow(Data)/20)),nrow(Data))),ncol=2))
  cal.WX  <- array(dim=c(B,nrow=length(c(seq(from=1,to=nrow(Data),by=round(nrow(Data)/20)),nrow(Data))),ncol=2))
  cal.WX.shrink  <- array(dim=c(B,nrow=length(c(seq(from=1,to=nrow(Data),by=round(nrow(Data)/20)),nrow(Data))),ncol=2))
  cal.WW  <- array(dim=c(B,nrow=length(c(seq(from=1,to=nrow(Data),by=round(nrow(Data)/20)),nrow(Data))),ncol=2))
  cal.WW.shrink  <- array(dim=c(B,nrow=length(c(seq(from=1,to=nrow(Data),by=round(nrow(Data)/20)),nrow(Data))),ncol=2))

  # Store seeds
  set.seed(57)
  seeds <- sample(1:100000000, size= B, replace=F)
  saveRDS(seeds, file= paste(filepath,"/seeds.rds", sep=""))
  
  
  # Bootstrap estimate of internal performance
  for (i in 1:B) {
    set.seed(seeds[i])
    k <- sample(1:nrow(Data), size = nrow(Data), replace = TRUE)
    bssample  <- data.frame(Data[k,] )
    
    bs.MX.lrm <- lrm(xmod,data=bssample)
    bs.MX.ridge <- Ridge(bssample,"X")
    bs.MW.lrm <- lrm(wmod,data=bssample)
    bs.MW.ridge <- Ridge(bssample,"W")
    
    # Performance no shrinkage
    lp.bs.XX <- LP(bssample,".lrm","X","X")
    lp.D.XX <- LP(Data,".lrm","X","X")
    Perf.bs.homog["XX",i,] <- Validation(lp.bs.XX,bssample$Y)
    Perf.D.homog["XX",i,] <- Validation(lp.D.XX,Data$Y)
    Optimism.homog["XX",i,] <- Perf.bs.homog["XX",i,] - Perf.D.homog["XX",i,]
    
    lp.bs.WW <- LP(bssample,".lrm","W","W")
    lp.D.WW <- LP(Data,".lrm","W","W")
    Perf.bs.homog["WW",i,] <- Validation(lp.bs.WW,bssample$Y)
    Perf.D.homog["WW",i,] <- Validation(lp.D.WW,Data$Y)
    Optimism.homog["WW",i,] <- Perf.bs.homog["WW",i,] - Perf.D.homog["WW",i,]

    lp.bs.XW <- LP(bssample,".lrm","X","W")
    lp.D.XW <- LP(Data,".lrm","X","W")
    Perf.D.heterog["XW",i,] <- Validation(lp.D.XW,Data$Y)
    Optimism.heterog["XW",i,] <- Perf.bs.homog["XX",i,] - Perf.D.heterog["XW",i,]
    
    lp.bs.WX <- LP(bssample,".lrm","W","X")
    lp.D.WX <- LP(Data,".lrm","W","X")
    Perf.D.heterog["WX",i,] <- Validation(lp.D.WX,Data$Y)
    Optimism.heterog["WX",i,] <- Perf.bs.homog["WW",i,] - Perf.D.heterog["WX",i,]
    
    Delta[i,"DeltaCalLXW"] <- Perf.D.heterog["XW",i,"CalL"] - Perf.D.homog["XX",i,"CalL"]
    Delta[i,"DeltaCalslopeXW"] <- Perf.D.heterog["XW",i,"Calslope"] - Perf.D.homog["XX",i,"Calslope"]
    Delta[i,"DeltaCXW"] <- Perf.D.heterog["XW",i,"C.stat"] - Perf.D.homog["XX",i,"C.stat"]
    Delta[i,"DeltaBrierXW"] <- Perf.D.heterog["XW",i,"Brier"] - Perf.D.homog["XX",i,"Brier"]
    Delta[i,"DeltaBrierscaledXW"] <- Perf.D.heterog["XW",i,"BrierScaled"] - Perf.D.homog["XX",i,"BrierScaled"]
    Delta[i,"DeltaR2XW"] <- Perf.D.heterog["XW",i,"R2"] - Perf.D.homog["XX",i,"R2"]
    
    Delta[i,"DeltaCalLWX"] <- Perf.D.heterog["WX",i,"CalL"] - Perf.D.homog["WW",i,"CalL"]
    Delta[i,"DeltaCalslopeWX"] <- Perf.D.heterog["WX",i,"Calslope"] - Perf.D.homog["WW",i,"Calslope"]
    Delta[i,"DeltaCWX"] <- Perf.D.heterog["WX",i,"C.stat"] - Perf.D.homog["WW",i,"C.stat"]
    Delta[i,"DeltaBrierWX"] <- Perf.D.heterog["WX",i,"Brier"] - Perf.D.homog["WW",i,"Brier"]
    Delta[i,"DeltaBrierscaledWX"] <- Perf.D.heterog["WX",i,"BrierScaled"] - Perf.D.homog["WW",i,"BrierScaled"]
    Delta[i,"DeltaR2WX"] <- Perf.D.heterog["WX",i,"R2"] - Perf.D.homog["WW",i,"R2"]
    
    # Performance shrinkage
    lp.bs.XX.ridge <- LP(bssample,".ridge","X","X")
    lp.D.XX.ridge <- LP(Data,".ridge","X","X")
    Perf.bs.homog.ridge["XX",i,] <- Validation(lp.bs.XX.ridge,bssample$Y)
    Perf.D.homog.ridge["XX",i,] <- Validation(lp.D.XX.ridge,Data$Y)
    Optimism.homog.ridge["XX",i,] <- Perf.bs.homog.ridge["XX",i,] - Perf.D.homog.ridge["XX",i,]
    
    lp.bs.WW.ridge <- LP(bssample,".ridge","W","W")
    lp.D.WW.ridge <- LP(Data,".ridge","W","W")
    Perf.bs.homog.ridge["WW",i,] <- Validation(lp.bs.WW.ridge,bssample$Y)
    Perf.D.homog.ridge["WW",i,] <- Validation(lp.D.WW.ridge,Data$Y)
    Optimism.homog.ridge["WW",i,] <- Perf.bs.homog.ridge["WW",i,] - Perf.D.homog.ridge["WW",i,]
    
    lp.bs.XW.ridge <- LP(bssample,".ridge","X","W")
    lp.D.XW.ridge <- LP(Data,".ridge","X","W")
    Perf.bs.heterog.ridge["XW",i,] <- Validation(lp.bs.XW.ridge,bssample$Y)
    Perf.D.heterog.ridge["XW",i,] <- Validation(lp.D.XW.ridge,Data$Y)
    Optimism.heterog.ridge["XW",i,] <- Perf.bs.heterog.ridge["XW",i,] - Perf.D.heterog.ridge["XW",i,]
    
    lp.bs.WX.ridge <- LP(bssample,".ridge","W","X")
    lp.D.WX.ridge <- LP(Data,".ridge","W","X")
    Perf.bs.heterog.ridge["WX",i,] <- Validation(lp.bs.WX.ridge,bssample$Y)
    Perf.D.heterog.ridge["WX",i,] <- Validation(lp.D.WX.ridge,Data$Y)
    Optimism.heterog.ridge["WX",i,] <- Perf.bs.heterog.ridge["WX",i,] - Perf.D.heterog.ridge["WX",i,]
    
    Delta.ridge[i,"DeltaCalLXW"] <- Perf.D.heterog.ridge["XW",i,"CalL"] - Perf.D.homog.ridge["XX",i,"CalL"]
    Delta.ridge[i,"DeltaCalslopeXW"] <- Perf.D.heterog.ridge["XW",i,"Calslope"] - Perf.D.homog.ridge["XX",i,"Calslope"]
    Delta.ridge[i,"DeltaCXW"] <- Perf.D.heterog.ridge["XW",i,"C.stat"] - Perf.D.homog.ridge["XX",i,"C.stat"]
    Delta.ridge[i,"DeltaBrierXW"] <- Perf.D.heterog.ridge["XW",i,"Brier"] - Perf.D.homog.ridge["XX",i,"Brier"]
    Delta.ridge[i,"DeltaBrierscaledXW"] <- Perf.D.heterog.ridge["XW",i,"BrierScaled"] - Perf.D.homog.ridge["XX",i,"BrierScaled"]
    Delta.ridge[i,"DeltaR2XW"] <- Perf.D.heterog.ridge["XW",i,"R2"] - Perf.D.homog.ridge["XX",i,"R2"]
    
    Delta.ridge[i,"DeltaCalLWX"] <- Perf.D.heterog.ridge["WX",i,"CalL"] - Perf.D.homog.ridge["WW",i,"CalL"]
    Delta.ridge[i,"DeltaCalslopeWX"] <- Perf.D.heterog.ridge["WX",i,"Calslope"] - Perf.D.homog.ridge["WW",i,"Calslope"]
    Delta.ridge[i,"DeltaCWX"] <- Perf.D.heterog.ridge["WX",i,"C.stat"] - Perf.D.homog.ridge["WW",i,"C.stat"]
    Delta.ridge[i,"DeltaBrierWX"] <- Perf.D.heterog.ridge["WX",i,"Brier"] - Perf.D.homog.ridge["WW",i,"Brier"]
    Delta.ridge[i,"DeltaBrierscaledWX"] <- Perf.D.heterog.ridge["WX",i,"BrierScaled"] - Perf.D.homog.ridge["WW",i,"BrierScaled"]
    Delta.ridge[i,"DeltaR2WX"] <- Perf.D.heterog.ridge["WX",i,"R2"] - Perf.D.homog.ridge["WW",i,"R2"]
    
    # Calibration plot input
    pred.XX <- plogis(lp.bs.XX)
    cal.XX[i,,1] <- lowess(pred.XX,bssample$Y, iter=0)$x[c(seq(from=1,to=nrow(bssample),by=round(nrow(bssample)/20)),nrow(bssample))]
    cal.XX[i,,2] <- lowess(pred.XX,bssample$Y, iter=0)$y[c(seq(from=1,to=nrow(bssample),by=round(nrow(bssample)/20)),nrow(bssample))]
    
    pred.XW <- plogis(lp.bs.XW)
    cal.XW[i,,1] <- lowess(pred.XW,bssample$Y, iter=0)$x[c(seq(from=1,to=nrow(bssample),by=round(nrow(bssample)/20)),nrow(bssample))]
    cal.XW[i,,2] <- lowess(pred.XW,bssample$Y, iter=0)$y[c(seq(from=1,to=nrow(bssample),by=round(nrow(bssample)/20)),nrow(bssample))]
    
    pred.WX <- plogis(lp.bs.WX)
    cal.WX[i,,1] <- lowess(pred.WX,bssample$Y, iter=0)$x[c(seq(from=1,to=nrow(bssample),by=round(nrow(bssample)/20)),nrow(bssample))]
    cal.WX[i,,2] <- lowess(pred.WX,bssample$Y, iter=0)$y[c(seq(from=1,to=nrow(bssample),by=round(nrow(bssample)/20)),nrow(bssample))]
    
    pred.WW <- plogis(lp.bs.WW)
    cal.WW[i,,1] <- lowess(pred.WW,bssample$Y, iter=0)$x[c(seq(from=1,to=nrow(bssample),by=round(nrow(bssample)/20)),nrow(bssample))]
    cal.WW[i,,2] <- lowess(pred.WW,bssample$Y, iter=0)$y[c(seq(from=1,to=nrow(bssample),by=round(nrow(bssample)/20)),nrow(bssample))]
    
    # Calibration plot input shrinkage
    pred.XX.shrink <- plogis(lp.bs.XX.ridge)
    cal.XX.shrink[i,,1] <- lowess(pred.XX.shrink,bssample$Y, iter=0)$x[c(seq(from=1,to=nrow(bssample),by=round(nrow(bssample)/20)),nrow(bssample))]
    cal.XX.shrink[i,,2] <- lowess(pred.XX.shrink,bssample$Y, iter=0)$y[c(seq(from=1,to=nrow(bssample),by=round(nrow(bssample)/20)),nrow(bssample))]
    
    pred.XW.shrink <- plogis(lp.bs.XW.ridge)
    cal.XW.shrink[i,,1] <- lowess(pred.XW.shrink,bssample$Y, iter=0)$x[c(seq(from=1,to=nrow(bssample),by=round(nrow(bssample)/20)),nrow(bssample))]
    cal.XW.shrink[i,,2] <- lowess(pred.XW.shrink,bssample$Y, iter=0)$y[c(seq(from=1,to=nrow(bssample),by=round(nrow(bssample)/20)),nrow(bssample))]
    
    pred.WX.shrink <- plogis(lp.bs.WX.ridge)
    cal.WX.shrink[i,,1] <- lowess(pred.WX.shrink,bssample$Y, iter=0)$x[c(seq(from=1,to=nrow(bssample),by=round(nrow(bssample)/20)),nrow(bssample))]
    cal.WX.shrink[i,,2] <- lowess(pred.WX.shrink,bssample$Y, iter=0)$y[c(seq(from=1,to=nrow(bssample),by=round(nrow(bssample)/20)),nrow(bssample))]
    
    pred.WW.shrink <- plogis(lp.bs.WW.ridge)
    cal.WW.shrink[i,,1] <- lowess(pred.WW.shrink,bssample$Y, iter=0)$x[c(seq(from=1,to=nrow(bssample),by=round(nrow(bssample)/20)),nrow(bssample))]
    cal.WW.shrink[i,,2] <- lowess(pred.WW.shrink,bssample$Y, iter=0)$y[c(seq(from=1,to=nrow(bssample),by=round(nrow(bssample)/20)),nrow(bssample))]
    
    print(i)
    Sys.sleep(0.01)
    flush.console()
  }
  
  # Store calibration plot output in global environment
  cal.XX <<- cal.XX
  cal.XW <<- cal.XW
  cal.WX <<- cal.WX
  cal.WW <<- cal.WW
  
  cal.XX.shrink <<- cal.XX.shrink
  cal.XW.shrink <<- cal.XW.shrink
  cal.WX.shrink <<- cal.WX.shrink
  cal.WW.shrink <<- cal.WW.shrink 
  
  # Save plotinput
  calinput <- list(cal.XX, cal.XW, cal.WX, cal.WW, cal.XX.shrink, cal.XW.shrink,cal.WX.shrink,cal.WW.shrink)
  saveRDS(calinput, file= paste(filepath,"/calinput.rds", sep=""))
  
  # Apparent performance, scaled Brier score
  BrierMax.X     <- mean(pred.DerX) * (1-mean(pred.DerX))
  BrierScaled.X  <- as.numeric((1-(MX.lrm$stats["Brier"]/ BrierMax.X)))
  
  BrierMax.W     <- mean(pred.DerW) * (1-mean(pred.DerW))
  BrierScaled.W  <- as.numeric((1-(MW.lrm$stats["Brier"]/ BrierMax.W)))
  
  # Apparent performance CI X
  ciRsq.X    <- quantile(Perf.bs.homog["XX",1:B,"R2"],c(0.025,0.975),names=F)
  sdRsq.X    <- sd(Perf.bs.homog["XX",1:B,"R2"])
  ciCstat.X  <- quantile(Perf.bs.homog["XX",1:B,"C.stat"],c(0.025,0.975),names=F)
  sdCstat.X  <- sd(Perf.bs.homog["XX",1:B,"C.stat"])
  ciBrier.X  <- quantile(Perf.bs.homog["XX",1:B,"Brier"],c(0.025,0.975),names=F)
  sdBrier.X  <- sd(Perf.bs.homog["XX",1:B,"Brier"])
  ciBrierScaled.X  <- quantile(Perf.bs.homog["XX",1:B,"BrierScaled"],c(0.025,0.975),names=F)
  sdBrierScaled.X  <- sd(Perf.bs.homog["XX",1:B,"BrierScaled"])
  
  # Apparent performance CI W
  ciRsq.W    <- quantile(Perf.bs.homog["WW",1:B,"R2"],c(0.025,0.975),names=F)
  sdRsq.W    <- sd(Perf.bs.homog["WW",1:B,"R2"])
  ciCstat.W  <- quantile(Perf.bs.homog["WW",1:B,"C.stat"],c(0.025,0.975),names=F)
  sdCstat.W  <- sd(Perf.bs.homog["WW",1:B,"C.stat"])
  ciBrier.W  <- quantile(Perf.bs.homog["WW",1:B,"Brier"],c(0.025,0.975),names=F)
  sdBrier.W  <- sd(Perf.bs.homog["WW",1:B,"Brier"])
  ciBrierScaled.W  <- quantile(Perf.bs.homog["WW",1:B,"BrierScaled"],c(0.025,0.975),names=F)
  sdBrierScaled.W  <- sd(Perf.bs.homog["WW",1:B,"BrierScaled"])
  
  # Create table model coefficients total sample
  Derperf <- data.frame(matrix(nrow=z+9,ncol=9))
  colnames(Derperf) <- c("Parameter", "Estimates of OR when X is used", "95% CI, lower","95% CI, upper",	
                         "Shrunk/penalized estimates of OR when X is used",
                         "Estimates of OR when W is used","95% CI, lower","95% CI, upper",
                         "Shrunk/penalized estimates of OR when W is used")
  Derperf[,1] <- c("N","Intercept", Hmisc::label(Data$X), Hmisc::label(Data$W),Hmisc::label(Data[,4:(3+z)]),"Model performance measures", "(Nagelkerke's) R2","C-statistic", "Brier score", "Scaled Brier score")
  Derperf[1,2] <- sum(MX.lrm$freq)
  Derperf[1,6] <- sum(MW.lrm$freq)
  Derperf[c(2,3,5:(4+z)),2:4] <- cbind(coef(MX.glm),confint(MX.glm))
  Derperf[c(2,4,5:(4+z)),6:8] <- cbind(coef(MW.glm),confint(MW.glm))
  Derperf[1,5] <- MX.ridge$nobs
  Derperf[1,9] <- MW.ridge$nobs
  Derperf[c(2,3,5:(4+z)),5] <- coef(MX.ridge,digits=10)
  Derperf[c(2,4,5:(4+z)),9] <- coef(MW.ridge)
  Derperf[nrow(Derperf)-3,2:4] <- c(MX.lrm$stats["R2"],ciRsq.X)
  Derperf[nrow(Derperf)-3,6:8] <- c(MW.lrm$stats["R2"],ciRsq.W)
  Derperf[nrow(Derperf)-2,2:4] <- c(MX.lrm$stats["C"],ciCstat.X)
  Derperf[nrow(Derperf)-2,6:8] <- c(MW.lrm$stats["C"],ciCstat.W)
  Derperf[nrow(Derperf)-1,2:4] <- c(MX.lrm$stats["Brier"],ciBrier.X)
  Derperf[nrow(Derperf)-1,6:8] <- c(MW.lrm$stats["Brier"],ciBrier.W)
  Derperf[nrow(Derperf),2:4] <- c(BrierScaled.X,ciBrierScaled.X)
  Derperf[nrow(Derperf),6:8] <- c(BrierScaled.W,ciBrierScaled.W)
  Derperf <<- Derperf
  print(xtable(Derperf,digits=10),file=file.path(filepath,"DerivationPerf_Tex.txt"), compress=F)
  
  # Optimisim corrected performance
  Optcorr.XX <- Optcor(Perf.D.homog,Optimism.homog,"X","X")
  write.table(Optcorr.XX, file = file.path(filepath,"OptcorrXX.txt"), row.names = T)
  Optcorr.XW <- Optcor(Perf.D.heterog,Optimism.heterog,"X","W")
  write.table(Optcorr.XW, file = file.path(filepath,"OptcorrXW.txt"), row.names = T)
  Optcorr.WX <- Optcor(Perf.D.heterog,Optimism.heterog,"W","X")
  write.table(Optcorr.WX, file = file.path(filepath,"OptcorrWX.txt"), row.names = T)
  Optcorr.WW <- Optcor(Perf.D.homog,Optimism.homog,"W","W")
  write.table(Optcorr.WW, file = file.path(filepath,"OptcorrWW.txt"), row.names = T)
  
  Optcorr.XX.ridge <- Optcor(Perf.D.homog.ridge,Optimism.homog.ridge,"X","X")
  write.table(Optcorr.XX.ridge, file = file.path(filepath,"OptcorrXXshrink.txt"), row.names = T)
  Optcorr.XW.ridge <- Optcor(Perf.D.heterog.ridge,Optimism.heterog.ridge,"X","W")
  write.table(Optcorr.XW.ridge, file = file.path(filepath,"OptcorrXWshrink.txt"), row.names = T)
  Optcorr.WX.ridge <- Optcor(Perf.D.heterog.ridge,Optimism.heterog.ridge,"W","X")
  write.table(Optcorr.WX.ridge, file = file.path(filepath,"OptcorrWXshrink.txt"), row.names = T)
  Optcorr.WW.ridge <- Optcor(Perf.D.homog.ridge,Optimism.homog.ridge,"W","W")
  write.table(Optcorr.WW.ridge, file = file.path(filepath,"OptcorrWWshrink.txt"), row.names = T)
  
  # Save optimism
  Opti(Optimism.homog,"X","X")
  Opti(Optimism.homog,"W","W")
  Opti(Optimism.heterog,"X","W")
  Opti(Optimism.heterog,"W","X")
  
  # Save deltas
  deltab <- data.frame(matrix(0,nrow=8,ncol=3))
  rownames(deltab) <- c("DeltaCXW",'DeltaCWX', "DeltaBrierXW","DeltaBrierWX", "DeltaBrierscaledXW","DeltaBrierscaledWX", "DeltaR2XW","DeltaR2WX")
  colnames(deltab) <- c("average","CIlow", "CIup")
  for(j in 1:nrow(deltab)){
    deltab[j,] <- c(mean(Delta[,j]),quantile(Delta[,j],c(0.025,0.975),names=F))
  }
  print(xtable(deltab,digits=10),file=file.path(filepath,"Deltas.txt"), compress=F)
  
  deltab.shrink <- data.frame(matrix(0,nrow=8,ncol=3))
  rownames(deltab.shrink) <- c("DeltaCXW","DeltaCWX","DeltaBrierXW","DeltaBrierWX", "DeltaBrierscaledXW","DeltaBrierscaledWX", "DeltaR2XW","DeltaR2WX")
  colnames(deltab.shrink) <- c("average","CIlow", "CIup")
  for(j in 1:nrow(deltab.shrink)){
    deltab.shrink[j,] <- c(mean(Delta.ridge[,j]),quantile(Delta.ridge[,j],c(0.025,0.975),names=F))
  }
  
  print(xtable(deltab.shrink,digits=10),file=file.path(filepath,"Deltas_shrink.txt"), compress=F)
}
