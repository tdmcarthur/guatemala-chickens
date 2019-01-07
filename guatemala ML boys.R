#--------------------------------------------------------------------------------------------------------------------
# This code was adapted from code written for the paper "Generic Machine Learning Discovery 
# and Classification Analysis of Heterogenous Treatment Effects in Randomized Experiments"
# by V. CHERNOZHUKOV, M. DEMIRER, E. DUFLO, I. FERNANDEZ-VAL
#--------------------------------------------------------------------------------------------------------------------
rm(list=ls(all=TRUE))
options(scipen=999)
data.path <- 'D:/Mullally,Conner/Documents/google drive/Guatemala ronda 2 2017/stata files/article stata work/stata data files'
code.path <- "D:/Mullally,Conner/Documents/google drive/Guatemala ronda 2 2017/stata files/article R scripts/MLInference-master/Heterogeneity"
#data.path <- 'C:/Users/Conner/Google Drive/Guatemala ronda 2 2017/stata files/article stata work/stata data files'
#code.path <- "C:/Users/Conner/Google Drive/Guatemala ronda 2 2017/stata files/article R scripts/MLInference-master/Heterogeneity"

library.path <- .libPaths(c("D:/Mullally,Conner/Documents/R/win-library/3.5"))
#library.path<-.libPaths(c("C:/Users/Conner/Documents/R/win-library/3.5"))

vec.pac= c("foreign", "quantreg", "gbm", "glmnet",
           "MASS", "rpart", "doParallel", "sandwich", "randomForest",
           "nnet", "matrixStats", "xtable", "readstata13", "car", "lfe", "doParallel",
           "caret", "foreach", "multcomp","cowplot", "iterators", "tcltk", "future", "dplyr", "lmtest")
install.packages(pkgs = vec.pac, dependencies = TRUE)
lapply(vec.pac, require, character.only = TRUE)

source(paste0(code.path, "/ML_Functions.R"))
source(paste0(code.path, "/edfreg.R"))
ptm <- proc.time()
seedstart <- 22859.60
if(exists('cl')) {
  stopCluster(cl)
}
no_cores <- future::availableCores() - 1
cl   <- makeCluster(no_cores, outfile="")
registerDoParallel(cl)

####################################### Load and Process Data  #######################################
data        <- read.dta13(paste(data.path,"/boyshet.dta",sep=""), convert.factors = "FALSE") # read in the data set

data$stratum  <- factor(data$stratum) # Turns stratum into factor variable
a           <- as.data.frame(model.matrix(~data$stratum-1)) 
colnames(a) <- (substring(names(a), 6, 14)) # replace column names with last 7 characters (i.e. drops data$ from name)
data <- cbind(data, a)
colnames(data)[which(colnames(data)=="stratum")] <- "vil_pair" # So as to not get confused between indicator containing which stratum community belongs to and the indicators for each separate strata

####################################### Inputs  #######################################

sim     <- 199   # number of sample splits (repitions for loop below)
K       <- 2       # number of folds (for each repition we split the data in K partsm K-1 of which are used to estimate scores)
p       <- 3       # number of groups (for splitting up treatment effects)
thres   <- 0.33    # quantile for most/least affected group
alpha   <- 0.05    # significance level

#  dimension of these three vectors should match. If dimension is greater than 1 the program runs heterogeneity estimation separately for each outcome variable
Y     <- c("zwei", "notunderweight", "zlen", "notstunted", "asf", "dds")     # vector of outcome variables
D     <- rep("treat", length(Y))  # vector of treatment variables

# specify cluster, fixed effect and partition
cluster      <- "cluster"       # if no cluster       use    cluster      <- "0"
fixed_effect <- "vil_pair"      # if no fixed_effect  use    fixed_effect <- "0". These should be the strata. 
partition    <- "cluster"       # if no partition     use    partition    <- "0". Each time we split data in K parts we sample from within variable specified here.

# create a vector of control variables
controls     <- c("edad_en_meses", "boy_zwei", "boy_zlen", "noboy", "gpsaltitude", "rain_maydec2015", "max30_maydec2015", "min_dist", "AME", "dep_ratio", "edu", "hhwealth", "dirtfloor", "micronutr", "bono", "womanwealth", "credit", "market_distance", "water", "electric", "hhsoc_capital", "womansoc_capital", "Lfoodcons_perc", "Lcalorias_percap", "LASFproteina_percap", "Legg_freq", "LFCS", "Legg_price", "Leggs_consumed", "Legg_production", "Lchicken_num")
controls     <- c(controls, names(data)[(substring( names(data), 1, 7)=="stratum")]) # adds stratum dummies to set of control variables
# for linear models (elastic net)
controls_linear <- c(controls, names(data)[(substring( names(data), 1, 4)=="int_")])
controls_linear

affected       <- c("edad_en_meses", "boy_zwei", "boy_zlen", "gpsaltitude", "rain_maydec2015", "max30_maydec2015", "AME", "dep_ratio", "edu", "hhwealth", "dirtfloor", "micronutr", "bono", "womanwealth", "credit", "market_distance", "water", "electric", "hhsoc_capital", "womansoc_capital", "Lfoodcons_perc", "Lcalorias_percap", "LASFproteina_percap", "Legg_freq", "LFCS", "Legg_price", "Leggs_consumed", "Legg_production", "Lchicken_num")

# generate formula for x, xl is for linear models

X <- ""

for(i in 1:length(controls)){
  X <- paste(X, controls[i], "+", sep = "")
}
X  <- substr(X, 1, nchar(X)-1)
XL <- ""
for(i in 1:length(controls_linear)){
  XL <- paste(XL, controls_linear[i], "+", sep = "")
}
XL  <- substr(XL, 1, nchar(XL)-1)
controls <- controls_linear
######################################################################################################

if(fixed_effect=="0" & cluster=="0"){
  data <- data[,c(Y, D,controls)]
}

if(fixed_effect=="0" & cluster!="0"){
  data <- data[,c(Y, D,controls, cluster)]
}

if(fixed_effect!="0" & cluster=="0"){
  data <- data[,c(Y, D,controls, fixed_effect)]
}

if(fixed_effect!="0" & cluster!="0"){
  data <- data[,c(Y, D, controls, cluster, fixed_effect)]
}

####################################### ML Inputs  #######################################

# svmPoly    : Support Vector Machines with Polynomial Kernel , package: kernlab, tuning parameters: degree (Polynomial Degree), scale (Scale), C (Cost)
# svmLinear  : Support Vector Machines with Linear Kernel , package: kernlab , C (Cost)
# svmLinear2 : Support Vector Machines with Linear Kernel , package: e1071 , cost (Cost)
# gbm        : Stochastic Gradient Boosting , package: gbm  , n.trees (# Boosting Iterations), interaction.depth (Max Tree Depth), shrinkage (Shrinkage), n.minobsinnode (Min. Terminal Node Size)
# glmnet     : Regularized Generalized Linear Models, package: glmnet , alpha (Mixing Percentage), lambda (Regularization Parameter)
# blackboost : Boosted Tree , package : mboost , mstop (#Trees), maxdepth (Max Tree Depth)
# nnet       : Neural Network , package : nnet  , size (#Hidden Units) , decay (Weight Decay)
# pcaNNet    : Neural Networks with Feature Extraction, package:nnet , size (#Hidden Units) , decay (Weight Decay)
# rpart      : CART, package:rpart, cp (Complexity Parameter)
# rf         : Random Forest,  package:randomForest, mtry (#Randomly Selected Predictors)


# Model names. For a list of available model names in caret package see: http://topepo.github.io/caret/available-models.html
# some available models given above

methods      <- c("glmnet", "rf")
method_names <- c("Elastic net", "Random Forest")

# A list of arguments for models used in the estimation
args         <- list(svmLinear2=list(type='eps-regression'), svmLinear=list(type='nu-svr'), svmPoly=list(type='nu-svr'), gbm=list(verbose=FALSE), rf=list(ntree=1000), gamboost=list(baselearner='btree'), avNNet=list(verbose = 0, linout = TRUE, trace = FALSE), pcaNNet=list(linout = TRUE, trace = FALSE, MaxNWts=100000, maxit=10000), nnet=list(linout = TRUE, trace = FALSE, MaxNWts=100000, maxit=10000))

methodML   <- c("repeatedcv","repeatedcv")   # resampling method for chosing tuning parameters. available options: boot, boot632, cv, LOOCV, LGOCV, repeatedcv, oob, none
tune       <- c(100,20)                                    # number of elements per parameter in the grid. the grid size is tune^{number of tuning parameters}. 
proces     <- c("range","range")                  # pre-processing method
select     <- c("best","best")                          # optimality criteria for choosing tuning parameter in cross validation. available options: best, oneSE, tolerance 
cv         <- c(2,2)                                          # the number of folds in cross-validation 
rep        <- c(2,2)                                         # number of iteration in repeated cross-validations 

# If there is a parameter of the model that user doesn't want to choose with cross validation, it should be set using tune_param variable. Below mtry of random forest is set to 5 
# for glmnet we want to choose both tuning parameters using cross validation so it is set to NULL

tune_param       <- list(0)
tune_param[[1]]  <- 0
tune_param[[2]]  <- 0

output_name <- paste("range","-","best", "-", 2, "-" ,2, "-",sim,sep="")
name        <- "EL1"

####################################### Estimation  #######################################

r <- foreach(t = 1:sim, .combine='cbind', .inorder=FALSE, .packages=vec.pac) %dopar% { 
  
  set.seed(t+seedstart);
  
  results       <- matrix(NA,6*length(Y), length(methods))
  results_het   <- matrix(NA,6*length(Y), length(methods))
  results_test  <- matrix(NA,24*length(Y), length(methods))
  table.who     <- matrix(NA, (length(affected)*18)*length(Y), length(methods))
  bestML        <- matrix(NA, 2*length(Y), length(methods))
  
  if(partition!="0"){
    ind <- createDataPartition(data[,partition], p = .5, list = FALSE)
    
    datause_raw <- as.data.frame(data[ ind,])
    dataout_raw <- as.data.frame(data[-ind,])
  }
  
  if(partition=="0"){
    split             <- runif(nrow(data))
    cvgroup           <- as.numeric(cut(split,quantile(split,probs = seq(0, 1, 1/K)),include.lowest = TRUE))  
    
    datause_raw       <- as.data.frame(data[cvgroup == 1,])
    dataout_raw       <- as.data.frame(data[cvgroup != 1,])  
  }
  
  for(i in 1:length(Y)){
    #i <- 6
    y      <- Y[i]
    d      <- D[i]
    
    datause   <- data.frame(datause_raw[complete.cases(datause_raw[, c(controls, y, d, affected)]),])
    dataout   <- data.frame(dataout_raw[complete.cases(dataout_raw[, c(controls, y, d, affected)]),])
    
    ind_u <- which(datause[,d]==1)         # treatment indicator
    
    for(l in 1:length(methods)){
      #  l <- 1
      if(methods[l]=="glmnet"){   x         <- XL     }
      if(methods[l]!="glmnet"){   x         <- X      }
      if(tune_param[[l]]==0){ f = NULL}
      if(tune_param[[l]]!=0){ f = tune_param[[l]]}
      
      form           <- as.formula(paste(y,"~",x,sep=""));
      
      ############ Estimate Scores using ML ############
      
      md_x         <- rep((nrow(datause[datause[,d]==1,]) + nrow(dataout[dataout[,d]==1,]))/(nrow(datause) + nrow(dataout)), nrow(dataout))  
      
      
      fitControl   <- trainControl(method = methodML[l], number = cv[l], repeats = rep[l], allowParallel = FALSE, verboseIter=FALSE, search="random", selectionFunction=select[l])
      arg          <- c(list(form=form, data = datause[ind_u,],  method = methods[l],  tuneGrid = f, trControl = fitControl, preProcess=proces[l], tuneLength=tune[l]), args[[methods[l]]])
      fit.yz1      <- suppressWarnings(do.call(caret::train, arg))
      my_z1x       <- predict(fit.yz1, newdata=dataout, type="raw")
      
      fitControl   <- trainControl(method = methodML[l], number = cv[l], repeats = rep[l], allowParallel = FALSE, verboseIter=FALSE, search="random", selectionFunction=select[l])
      arg          <- c(list(form=form, data = datause[-ind_u,],  method = methods[l], tuneGrid = f, trControl = fitControl, preProcess=proces[l], tuneLength=tune[l]), args[[methods[l]]])
      fit.yz0      <- suppressWarnings(do.call(caret::train, arg))
      my_z0x       <- predict(fit.yz0, newdata=dataout, type="raw")
      
      ind           <- (md_x>0.01 & md_x<0.99)
      dataout       <- dataout[ind, ]
      my_z1x        <- my_z1x[ind]
      my_z0x        <- my_z0x[ind]
      md_x          <- md_x[ind]
      
      ############################################# ATE by groups ############################################# 
      
      B   <- my_z0x
      S   <- (my_z1x - my_z0x)
      
      S2        <- S+runif(length(S), 0, 0.00001)
      breaks    <- quantile(S2, seq(0,1, (1/3)),  include.lowest =T)
      breaks[1] <- breaks[1] - 0.001
      breaks[4] <- breaks[4] + 0.001
      SG        <- cut(S2, breaks = breaks)
      
      SGX       <- model.matrix(~-1+SG)
      DSG       <- data.frame(as.numeric(I(as.numeric(dataout[,d])-md_x))*SGX)
      
      colnames(DSG) <- c("G1", "G2", "G3")
      dataout[,c("B", "S", "G1", "G2", "G3", "weight")] <- cbind(B, S, DSG$G1, DSG$G2, DSG$G3, as.numeric((1/(md_x*(1-md_x)))))
      
      if(var(dataout$B)==0) {dataout$B <- dataout$B + rnorm(length(dataout$B),  mean=0, sd=0.1) }
      if(var(dataout$S)==0) {dataout$S <- dataout$S + rnorm(length(dataout$S),  mean=0, sd=0.1) }
      
      form1 <- as.formula(paste(y, "~", "B+S+G1+G2+G3 | ", fixed_effect, "| 0 |", cluster, sep=""))
      
      a <- tryCatch({
        a <- felm(form1, data=dataout, weights=dataout$weight, keepCX = TRUE)  
      },error=function(e){
        cat("ERROR :",methods[l], t, i, "\n")
        form1  <- as.formula(paste(y, "~", "G1+G2+G3 | ", fixed_effect, "| 0 |", cluster, sep=""))
        reg    <- felm(form1, data=dataout, weights=dataout$weight, keepCX = TRUE)  
        return(reg)
      }, warning = function(war) {
        cat("WARNING :",methods[l], t, i, "\n")
        form1  <- as.formula(paste(y, "~", "G1+G2+G3 | ", fixed_effect, "| 0 |", cluster, sep=""))
        reg    <- felm(form1, data=dataout, weights=dataout$weight, keepCX = TRUE)  
        return(reg)
      })
      reg   <- a
      
      coef <- (summary(reg)$coefficients['G3',1])
      edfreg.ret <- edfreg(reg, "G3 = 0", alpha)
      pval <- edfreg.ret$p.value
      confidence.interval <- edfreg.ret$confidence.interval
      edf <- edfreg.ret$edf # xxx Conner added this code (and similar code) to get edf, which is used later for MHT adjustments
      results_test[(1+(i-1)*24):(6+((i-1)*24)),l]  <- c(coef, confidence.interval, (as.numeric(coef<0)*(pval/2) + as.numeric(coef>0)*(1-pval/2)),(as.numeric(coef<0)*(1-pval/2) + as.numeric(coef>0)*(pval/2)), edf)
      
      coef <- (summary(reg)$coefficients['G2',1])
      edfreg.ret <- edfreg(reg, "G2 = 0", alpha)
      pval <- edfreg.ret$p.value
      confidence.interval <- edfreg.ret$confidence.interval
      edf <- edfreg.ret$edf # xxx Conner added this code (and similar code) to get edf, which is used later for MHT adjustments
      results_test[(7+(i-1)*24):(12+((i-1)*24)),l]  <- c(coef, confidence.interval, (as.numeric(coef<0)*(pval/2) + as.numeric(coef>0)*(1-pval/2)),(as.numeric(coef<0)*(1-pval/2) + as.numeric(coef>0)*(pval/2)), edf)      
      
      coef <- (summary(reg)$coefficients['G1',1])
      edfreg.ret <- edfreg(reg, "G1 = 0", alpha)
      pval <- edfreg.ret$p.value
      confidence.interval <- edfreg.ret$confidence.interval
      edf <- edfreg.ret$edf # xxx Conner added this code (and similar code) to get edf, which is used later for MHT adjustments.
      results_test[(13+(i-1)*24):(18+((i-1)*24)),l] <- c(coef, confidence.interval,(as.numeric(coef<0)*(pval/2) + as.numeric(coef>0)*(1-pval/2)),(as.numeric(coef<0)*(1-pval/2) + as.numeric(coef>0)*(pval/2)), edf)    
      
      coef <- (summary(reg)$coefficients['G3',1]) - (summary(reg)$coefficients['G1',1])
      edfreg.ret <- edfreg(reg, "G3 - G1 = 0", alpha)
      pval <- edfreg.ret$p.value
      confidence.interval <- edfreg.ret$confidence.interval
      edf <- edfreg.ret$edf # xxx Conner added this code (and similar code) to get edf, which is used later for MHT adjustments
      results_test[(19+(i-1)*24):(24+((i-1)*24)),l] <- c(coef, confidence.interval,(as.numeric(coef<0)*(pval/2) + as.numeric(coef>0)*(1-pval/2)),(as.numeric(coef<0)*(1-pval/2) + as.numeric(coef>0)*(pval/2)), edf)    
      
      mean <- summary(reg)$coef[c('G1','G2','G3'),1]
      # Goodness of fit: maximize variation in outcomes explained by terciles
      bestML[(1+(i-1)*2),l]  <- (sum(mean^2)/3)
      
      ################################### Best Linear Prediction Regression  ################################### 
      
      Sd            <- dataout$S- mean(dataout$S)
      dataout$S_ort <- I((as.numeric(dataout[,d])-md_x)*Sd)
      dataout$d_ort <- I((as.numeric(dataout[,d])-md_x))
      
      form1 <- as.formula(paste(y, "~", "B+S+d_ort+S_ort| ", fixed_effect, "| 0 |", cluster, sep=""))
      
      a  <- tryCatch({
        a  <- felm(form1, data=dataout, weights=dataout$weight, keepCX = TRUE)   
      },error=function(e){
        cat("ERROR2 :",methods[l], t, i, "\n")
        form1 <- as.formula(paste(y, "~", "d_ort+S_ort| ", fixed_effect, "| 0 |", cluster, sep=""))
        reg   <- felm(form1, data=dataout, weights=dataout$weight, keepCX = TRUE)  
        return(reg)
      }, warning = function(war) {
        cat("WARNING2 :",methods[l], t, i, "\n")
        form1 <- as.formula(paste(y, "~", "d_ort+S_ort| ", fixed_effect, "| 0 |", cluster, sep=""))
        reg   <- felm(form1, data=dataout, weights=dataout$weight, keepCX = TRUE)  
        return(reg)
      })
      reg <- a 
      
      coef <- (summary(reg)$coefficients['d_ort',1])
      edfreg.ret <- edfreg(reg, "d_ort = 0", alpha)
      pval <- edfreg.ret$p.value
      confidence.interval <- edfreg.ret$confidence.interval
      edf <- edfreg.ret$edf
      results[(1+(i-1)*6):(i*6),l]      <-c(coef, confidence.interval,  (as.numeric(coef<0)*(pval/2) + as.numeric(coef>0)*(1-pval/2)),(as.numeric(coef<0)*(1-pval/2) + as.numeric(coef>0)*(pval/2)), edf) # xxx changed to accomodate saving df
      coef <- (summary(reg)$coefficients['S_ort',1])
      
      edfreg.ret <- edfreg(reg, "S_ort = 0", alpha)
      pval <- edfreg.ret$p.value
      confidence.interval <- edfreg.ret$confidence.interval
      edf <- edfreg.ret$edf
      results_het[(1+(i-1)*6):(i*6),l] <- c(coef, confidence.interval,  (as.numeric(coef<0)*(pval/2) + as.numeric(coef>0)*(1-pval/2)),(as.numeric(coef<0)*(1-pval/2) + as.numeric(coef>0)*(pval/2)), edf)
      bestML[(2+(i-1)*2),l]      <- abs(summary(reg)$coefficients['S_ort',1])*sqrt(var(dataout$S))
      
      ################################################ Most/Least Affected  ################################################ 
      
      high.effect     <- quantile(dataout$S, 1-thres);
      low.effect      <- quantile(dataout$S, thres);
      dataout$h       <- as.numeric(dataout$S>high.effect)
      dataout$l       <- as.numeric(dataout$S<low.effect)
      
      if(var(dataout$h)==0){dataout$h <- as.numeric(runif(length(dataout$h))<0.1)}
      if(var(dataout$l)==0){dataout$l <- as.numeric(runif(length(dataout$l))<0.1)}
      
      for(m in 1:length(affected)){
        a  <- tryCatch({
          form  <- paste(affected[m],"~h+l-1", sep="")
          reg   <- lm(form, data=dataout[(dataout$h==1)| (dataout$l==1),])    
          coef  <- reg$coefficients['h'] - reg$coefficients['l']
          test  <- glht(reg, linfct = c("h-l==0"))
          
          form2  <- paste(affected[m],"~h", sep="")
          reg2   <- lm(form2, data=dataout[(dataout$h==1)| (dataout$l==1),])    
          test2 <- coeftest(reg2, vcov = vcovHC, type = 'HC1')
          
          coef  <- (summary(reg)$coefficients['h',1])
          pval  <- (summary(reg)$coefficients['h',4])
          df1    <- reg$df.residual
          res1  <- c(coef, confint(reg, 'h', level = 1-alpha)[1:2], (as.numeric(coef<0)*(pval/2) + as.numeric(coef>0)*(1-pval/2)),(as.numeric(coef<0)*(1-pval/2) + as.numeric(coef>0)*(pval/2)), df1)
          
          coef  <- (summary(reg)$coefficients['l',1])
          pval  <- (summary(reg)$coefficients['l',4])
          df2    <- reg$df.residual
          res2  <- c(coef, confint(reg, 'l', level = 1-alpha)[1:2], (as.numeric(coef<0)*(pval/2) + as.numeric(coef>0)*(1-pval/2)),(as.numeric(coef<0)*(1-pval/2) + as.numeric(coef>0)*(pval/2)), df2)
          
          coef  <- (summary(reg)$coefficients['h',1]) - (summary(reg)$coefficients['l',1])
          pval <- as.numeric(test2[2,4])
          df3 <- test$df
          res3  <- c((confint(test,level = 1-alpha))$confint[1:3], (as.numeric(coef<0)*(pval/2) + as.numeric(coef>0)*(1-pval/2)),(as.numeric(coef<0)*(1-pval/2) + as.numeric(coef>0)*(pval/2)), df3)
          a     <- c(res1, res2, res3)
          
        },error=function(e){
          cat("ERROR3 :",methods[l], t, i, "\n")
          
          res1  <- c(mean(dataout[(dataout$h==1),affected[m]]), mean(dataout[(dataout$h==1),affected[m]]), mean(dataout[(dataout$h==1),affected[m]]), 0.5, 0.5, df1)
          res2  <- c(mean(dataout[(dataout$l==1),affected[m]]), mean(dataout[(dataout$l==1),affected[m]]), mean(dataout[(dataout$l==1),affected[m]]), 0.5, 0.5, df2)
          res3  <- c((res1[1] - res2[1]), (res1[1] - res2[1]), (res1[1] - res2[1]), 0.5 , 0.5, df3)
          a     <- c(res1, res2, res3)
          return(a)
        })
        table.who[((i-1)*length(affected)*18+(m-1)*18+1):((i-1)*length(affected)*18+(m)*18),l]   <- a #xxx changed this to accomodate saving degrees of freedom
      }
    }
  }
  res <- c(as.vector(results_test), as.vector(results), as.vector(results_het), as.vector(bestML), as.vector(table.who))
  dim(res)
  print(t)
  r <- data.frame(res)
}
ptm

#write.csv(r,(paste(data.path,"/boys_ml_output.csv",sep=""))) # save it to a csv
write.csv(r,"boys_ml_output.csv") # save it to a csv

