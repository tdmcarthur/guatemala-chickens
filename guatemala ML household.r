#--------------------------------------------------------------------------------------------------------------------
# This code was adapted from code written for the paper "Generic Machine Learning Discovery 
# and Classification Analysis of Heterogenous Treatment Effects in Randomized Experiments"
# by V. CHERNOZHUKOV, M. DEMIRER, E. DUFLO, I. FERNANDEZ-VAL
#--------------------------------------------------------------------------------------------------------------------

# This program returns three outputs
# 1) A plot for each outcome variable reporting ATE by groups
# 2) het_results: Latex table reporting ATE and heterogeneity loading coefficient 
# 3) test_results: A latex table reporting  estimated ATE_group1 - ATE_group5 and its p-value
# 4) best: A latex table reporting best ML methods
# 5) MSE: A latex table of mean squared errors from tuning
rm(list=ls(all=TRUE))

setwd("D:/Mullally,Conner/Documents/google drive/Guatemala ronda 2 2017/stata files/article R scripts/MLInference-master/Heterogeneity")
data.path <- 'D:/Mullally,Conner/Documents/google drive/Guatemala ronda 2 2017/stata files/article stata work/stata data files'


library.path <- .libPaths(c("D:/Mullally,Conner/Documents/R/win-library/3.5"))
.libPaths(c("D:/Mullally,Conner/Documents/R/win-library/3.5"))

vec.pac= c("foreign", "quantreg", "gbm", "glmnet",
           "MASS", "rpart", "doParallel", "sandwich", "randomForest",
           "nnet", "matrixStats", "xtable", "readstata13", "car", "lfe", "doParallel",
           "caret", "foreach", "multcomp","cowplot", "iterators", "tcltk", "future", "dplyr")
# install.packages(pkgs = vec.pac, dependencies = TRUE)

lapply(vec.pac, require, character.only = TRUE)

source("ML_Functions.R")
ptm <- proc.time()

set.seed(1211);
if(exists('cl')) {
  stopCluster(cl)
}
no_cores <- future::availableCores() - 1
cl   <- makeCluster(no_cores, outfile="")
registerDoParallel(cl)

####################################### Load and Process Data  #######################################
data        <- read.dta13(paste(data.path,"/hhhet.dta",sep=""), convert.factors = "FALSE") # read in the data set

# Line below was extremely slow so I did data collapse in Stata

data$stratum  <- factor(data$stratum) # Turns stratum into factor variable
a           <- as.data.frame(model.matrix(~data$stratum-1)) 
colnames(a) <- (substring(names(a), 6, 14)) # replace columns names with last 7 characters (i.e. drops data$ from name)
data <- cbind(data, a)

####################################### Inputs  #######################################

sim     <- 100     # number of sample splits (repitions for loop below)
K       <- 2       # number of folds (for each repition we split the data in K partsm K-1 of which are used to estimate scores)
p       <- 3       # number of groups (for splitting up treatment effects)
thres   <- 0.33    # quantile for most/least affected group
alpha   <- 0.05    # significance level

#  dimension of these three vectors should match. If dimension is greater than 1 the program runs heterogeneity estimation separately for each outcome variable
names <- c("Annual expenditure per adult male equivalent (log)", "Annual food expenditure per adult male equivalent (log)", "Daily calories per adult male equivalent (log)", "Daily grams of animal protein per adult male equivalent", "Daily servings of eggs", "Eggs consumed per day per adult male equivalent (log)", "Food consumption score", "Chickens owned", "Naked-neck chickens owned", "Eggs produced in last six months (log)")    # vector of labels for outcome variables
Y     <- c("gastos_perAME", "foodcons_perc", "calorias_percap", "ASFproteina_percap", "egg_freq", "eggs_consumed", "FCS", "chicken_num", "pelucas_num", "egg_production")     # vector of outcome variables
D     <- rep("treat", length(Y))  # vector of treatment variables

# specify cluster, fixed effect and partition
cluster      <- "cluster"       # if no cluster       use    cluster      <- "0"
fixed_effect <- "stratum"         # if no fixed_effect  use    fixed_effect <- "0". These should be the strata. 
partition    <- "cluster"       # if no partition     use    partition    <- "0". Each time we split data in K parts we sample from within variable specified here.

# create a vector of control variables
controls     <- c("gpsaltitude", "min_dist", "rain_maydec2015", "max30_maydec2015", "AME", "dep_ratio", "edu", "hhwealth", "dirtfloor", "hhsoc_capital", "credit", "market_distance", "electric", "water", "Lfoodcons_perc", "Lcalorias_percap", "LASFproteina_percap", "LFCS", "Leggs_consumed", "Lmaize_production","Lfrijol_production", "Legg_price", "Legg_production", "Lchicken_num", "Lpelucas_num", "chicken_shelter", "womanwealth", "womansoc_capital")
controls     <- c(controls, names(data)[(substring( names(data), 1, 5)=="paire")]) # adds the village-pair fixed effects to set of control variables

affected       <- c("gpsaltitude", "min_dist", "rain_maydec2015", "max30_maydec2015", "AME", "dep_ratio", "edu", "hhwealth", "dirtfloor", "hhsoc_capital", "credit", "market_distance", "electric", "water", "Lfoodcons_perc", "Lcalorias_percap", "LASFproteina_percap", "LFCS", "Leggs_consumed", "Lmaize_production","Lfrijol_production", "Legg_price", "Legg_production", "Lchicken_num", "Lpelucas_num", "chicken_shelter", "womanwealth", "womansoc_capital")
names_affected <- c("Meters above sea level", "Distance to weather station (kilometers)", "Rainfall at closest weather station below median for 2015 crop season (0/1)", "Days with maximum temperature $>$ \ang{30}C during 2015 crop season above median", "Household size (adult male equivalents)", "Dependency ratio", "Average years of education, 12 years of age and older", "Wealth", "Dwelling has dirt floor", "Social capital index, household", "Had credit at baseline (0/1)", "Time in minutes to arrive at nearest market", "Connected to electricity network (0/1)", "Connected to water network (0/1)", "Annual food expenditure per adult male equivalent (lag) (log)", "Daily calories per adult male equivalent (lag) (log)", "Daily grams of animal protein per adult male equivalent (lag) (log)", "Food consumption score (lag) (log)", "Eggs consumed per day per adult male equivalent (lag) (log)", "Maize production, pounds (lag) (log)", "Bean production, pounds (lag) (log)", "egg_price (lag) (log)", "Eggs produced in last six months (lag) (log)", "Chickens owned (lag) (log)", "Naked-neck chickens owned (lag) (log)", "Chickens have housing (0/1)", "Women's share of wealth", "Social capital index, women")

# generate formula for x, xl is for linear models

X <- ""

for(i in 1:length(controls)){
  X <- paste(X, controls[i], "+", sep = "")
}
X  <- substr(X, 1, nchar(X)-1)
XL <- paste("(", X , ")", sep="")
XL <- X

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
# methods      <- c("glmnet", "gbm", "pcaNNet", "rf")
# method_names <- c("Elastic Net", "Boosting", "Nnet", "Random Forest")

methods      <- c("glmnet", "rf")
method_names <- c("Elastic net", "Random Forest")

# A list of arguments for models used in the estimation
args         <- list(svmLinear2=list(type='eps-regression'), svmLinear=list(type='nu-svr'), svmPoly=list(type='nu-svr'), gbm=list(verbose=FALSE), rf=list(ntree=1000), gamboost=list(baselearner='btree'), avNNet=list(verbose = 0, linout = TRUE, trace = FALSE), pcaNNet=list(linout = TRUE, trace = FALSE, MaxNWts=100000, maxit=10000), nnet=list(linout = TRUE, trace = FALSE, MaxNWts=100000, maxit=10000))

# methodML   <- c("repeatedcv", "repeatedcv", "repeatedcv", "none")   # resampling method for chosing tuning parameters. available options: boot, boot632, cv, LOOCV, LGOCV, repeatedcv, oob, none
# tune       <- c(100, 20, 20, NA)                                    # number of elements per parameter in the grid. the grid size is tune^{number of tuning parameters}. 
# proces     <- c("range", "range","range", "range")                  # pre-processing method
# select     <- c("best", "best","best", NA)                          # optimality criteria for choosing tuning parameter in cross validation. available options: best, oneSE, tolerance 
# cv         <- c(2, 2,2, 2)                                          # the number of folds in cross-validation 
# rep        <- c(2, 2,2, NA)                                         # number of iteration in repeated cross-validations 

methodML   <- c("repeatedcv","repeatedcv")   # resampling method for chosing tuning parameters. available options: boot, boot632, cv, LOOCV, LGOCV, repeatedcv, oob, none
tune       <- c(100,20)                                    # number of elements per parameter in the grid. the grid size is tune^{number of tuning parameters}. 
proces     <- c("range","range")                  # pre-processing method
select     <- c("best","best")                          # optimality criteria for choosing tuning parameter in cross validation. available options: best, oneSE, tolerance 
cv         <- c(2,2)                                          # the number of folds in cross-validation 
rep        <- c(2,2)                                         # number of iteration in repeated cross-validations 

# If there is a parameter of the model that user doesn't want to choose with cross validation, it should be set using tune_param variable. Below mtry of random forest is set to 5 
# for glmnet we want to choose both tuning parameters using cross validation so it is set to NULL

# tune_param       <- list(0)
# tune_param[[1]]  <- data.frame(alp=1)
# tune_param[[2]]  <- 0
# tune_param[[3]]  <- 0
# tune_param[[4]]  <- data.frame(mtry=5)

tune_param       <- list(0)
#tune_param[[1]]  <- data.frame(mtry=5)
tune_param[[1]]  <- 0
tune_param[[2]]  <- 0

output_name <- paste("range","-","best", "-", 2, "-" ,2, "-",sim,sep="")
name        <- "EL1"

####################################### Estimation  #######################################

r <- foreach(t = 1:sim, .combine='cbind', .inorder=FALSE, .packages=vec.pac) %dopar% { 
  #t <- 1
  set.seed(t);
  
  results       <- matrix(NA,5*length(Y), length(methods))
  results_het   <- matrix(NA,5*length(Y), length(methods))
  results_test  <- matrix(NA,15*length(Y), length(methods))
  # results_group <- matrix(NA, 15*length(Y), length(methods))
  results_group <- matrix(NA, 9*length(Y), length(methods))
  table.who     <- matrix(NA, (length(affected)*15)*length(Y), length(methods))
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
    #i <- 1
    y      <- Y[i]
    d      <- D[i]
    
    datause   <- data.frame(datause_raw[complete.cases(datause_raw[, c(controls, y, d, affected)]),])
    dataout   <- data.frame(dataout_raw[complete.cases(dataout_raw[, c(controls, y, d, affected)]),])
    
    ind_u <- which(datause[,d]==1)         # treatment indicator
    
    for(l in 1:length(methods)){
      #l <- 1
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
        a <- felm(form1, data=dataout, weights=dataout$weight)  
      },error=function(e){
        cat("ERROR :",methods[l], t, i, "\n")
        form1  <- as.formula(paste(y, "~", "G1+G2+G3 | ", fixed_effect, "| 0 |", cluster, sep=""))
        reg    <- felm(form1, data=dataout, weights=dataout$weight)  
        return(reg)
      }, warning = function(war) {
        cat("WARNING :",methods[l], t, i, "\n")
        form1  <- as.formula(paste(y, "~", "G1+G2+G3 | ", fixed_effect, "| 0 |", cluster, sep=""))
        reg    <- felm(form1, data=dataout, weights=dataout$weight)  
        return(reg)
      })
      reg   <- a
      
      coef <- (summary(reg)$coefficients['G3',1])
      pval <- (summary(reg)$coefficients['G3',4])
      results_test[(1+(i-1)*15):(5+((i-1)*15)),l]  <- c(coef, confint(reg, 'G3', level = 1-alpha)[1:2], (as.numeric(coef<0)*(pval/2) + as.numeric(coef>0)*(1-pval/2)),(as.numeric(coef<0)*(1-pval/2) + as.numeric(coef>0)*(pval/2)))
      
      coef <- (summary(reg)$coefficients['G1',1])
      pval <- (summary(reg)$coefficients['G1',4])
      results_test[(6+(i-1)*15):(10+((i-1)*15)),l]  <- c(coef, confint(reg, 'G1', level = 1-alpha)[1:2], (as.numeric(coef<0)*(pval/2) + as.numeric(coef>0)*(1-pval/2)),(as.numeric(coef<0)*(1-pval/2) + as.numeric(coef>0)*(pval/2)) )
      
      test <- glht(reg, linfct = c("G3-G1==0"))
      coef <- (summary(reg)$coefficients['G3',1]) - (summary(reg)$coefficients['G1',1])
      pval <- summary(test)$test$pvalues[1]
      results_test[(11+(i-1)*15):(15+((i-1)*15)),l] <- c((confint(test,level = 1-alpha))$confint[1:3],(as.numeric(coef<0)*(pval/2) + as.numeric(coef>0)*(1-pval/2)),(as.numeric(coef<0)*(1-pval/2) + as.numeric(coef>0)*(pval/2)))
      # results_test: rows 1 to 15 are first outcome, 16 to 30 second outcome. COlumn 1 glmnet, column 2 rf.
      # later when it is vectorized, first 15 will be glmnet first oucome, next 15 glmnet second outcome, next 15 rf first outcome, etc. 60 elements.
      mean <- summary(reg)$coef[c('G1','G2','G3'),1]
      sd   <- summary(reg)$coef[c('G1','G2','G3'),2]
      
      crit <- qnorm(1-alpha/(p))
      
      #results_group[((i-1)*15+1):((i-1)*15+5),l]   <- sort(mean)
      results_group[((i-1)*9+1):((i-1)*9+3),l]   <- sort(mean)
      results_group[((i-1)*9+4):((i-1)*9+6),l]  <- sort(mean +crit*sd)
      results_group[((i-1)*9+7):((i-1)*9+9),l] <- sort(mean -crit*sd)
      # 36 x 2 when complete with 3 quantiles
      # With 5 (3) groups: results_group: rows 1 - 5 (1-3) are the coefficients for each GATE. rows 6 to 10 (4-6) 
      # are the 95% CI ub for each GATE. rows 11 to 15 (7 - 9)are the 95% CI lb for each GATE. 
      # Repeats for next outcome, down to row 60 (36). Each ML method has own column.
      # so when vectorized results_group will have 120 (72) elements. 
      
      bestML[(1+(i-1)*2),l]  <- (sum(mean^2)/5)
      
      ################################### Best Linear Prediction Regression  ################################### 
      
      Sd            <- dataout$S- mean(dataout$S)
      dataout$S_ort <- I((as.numeric(dataout[,d])-md_x)*Sd)
      dataout$d_ort <- I((as.numeric(dataout[,d])-md_x))
      
      form1 <- as.formula(paste(y, "~", "B+S+d_ort+S_ort| ", fixed_effect, "| 0 |", cluster, sep=""))
      
      a  <- tryCatch({
        a  <- felm(form1, data=dataout, weights=dataout$weight)   
      },error=function(e){
        cat("ERROR2 :",methods[l], t, i, "\n")
        form1 <- as.formula(paste(y, "~", "d_ort+S_ort| ", fixed_effect, "| 0 |", cluster, sep=""))
        reg   <- felm(form1, data=dataout, weights=dataout$weight)  
        return(reg)
      }, warning = function(war) {
        cat("WARNING2 :",methods[l], t, i, "\n")
        form1 <- as.formula(paste(y, "~", "d_ort+S_ort| ", fixed_effect, "| 0 |", cluster, sep=""))
        reg   <- felm(form1, data=dataout, weights=dataout$weight)  
        return(reg)
      })
      reg <- a 
      
      coef <- (summary(reg)$coefficients['d_ort',1])
      pval <- (summary(reg)$coefficients['d_ort',4])
      results[(1+(i-1)*5):(i*5),l]      <-c(coef, confint(reg, 'd_ort', level = 1-alpha)[1:2],  (as.numeric(coef<0)*(pval/2) + as.numeric(coef>0)*(1-pval/2)),(as.numeric(coef<0)*(1-pval/2) + as.numeric(coef>0)*(pval/2)))
      # results: first 5 rows are ATE of outcome 1: coef, 95% CI lb and ub, pval1, pval2. Next 5 are ATE of second outcome. Each ML method in its own column.
      # 20 x 2 matrix that has 40 elements when vectorized.
      coef <- (summary(reg)$coefficients['S_ort',1])
      pval <- (summary(reg)$coefficients['S_ort',4])
      results_het[(1+(i-1)*5):(i*5),l] <- c(coef, confint(reg, 'S_ort', level = 1-alpha)[1:2],  (as.numeric(coef<0)*(pval/2) + as.numeric(coef>0)*(1-pval/2)),(as.numeric(coef<0)*(1-pval/2) + as.numeric(coef>0)*(pval/2)))
      # results_het: results for ATE heterogeneity coefficient. Same pattern as results.
      bestML[(2+(i-1)*2),l]      <- abs(summary(reg)$coefficients['S_ort',1])*sqrt(var(dataout$S))
      # bestML: abs(heterogeneity coefficient)/standard error of heterogeneity coefficient. THis is second row. 
      # In first row we have maximizing the proportion of variation in outcome explained by the different groups (split into quantiles by size of treatment effect)
      # Repeat for each outcome.
      # One column for each ML.
      
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
          
          coef  <- (summary(reg)$coefficients['h',1])
          pval  <- (summary(reg)$coefficients['h',4])
          res1  <- c(coef, confint(reg, 'h', level = 1-alpha)[1:2], (as.numeric(coef<0)*(pval/2) + as.numeric(coef>0)*(1-pval/2)),(as.numeric(coef<0)*(1-pval/2) + as.numeric(coef>0)*(pval/2)))
          
          coef  <- (summary(reg)$coefficients['l',1])
          pval  <- (summary(reg)$coefficients['l',4])
          res2  <- c(coef, confint(reg, 'l', level = 1-alpha)[1:2], (as.numeric(coef<0)*(pval/2) + as.numeric(coef>0)*(1-pval/2)),(as.numeric(coef<0)*(1-pval/2) + as.numeric(coef>0)*(pval/2)))
          
          coef  <- (summary(reg)$coefficients['h',1]) - (summary(reg)$coefficients['l',1])
          pval  <- summary(test)$test$pvalues[1]
          res3  <- c((confint(test,level = 1-alpha))$confint[1:3], (as.numeric(coef<0)*(pval/2) + as.numeric(coef>0)*(1-pval/2)),(as.numeric(coef<0)*(1-pval/2) + as.numeric(coef>0)*(pval/2)))
          a     <- c(res1, res2, res3)
          
        },error=function(e){
          cat("ERROR3 :",methods[l], t, i, "\n")
          
          res1  <- c(mean(dataout[(dataout$h==1),affected[m]]), mean(dataout[(dataout$h==1),affected[m]]), mean(dataout[(dataout$h==1),affected[m]]), 0.5, 0.5)
          res2  <- c(mean(dataout[(dataout$l==1),affected[m]]), mean(dataout[(dataout$l==1),affected[m]]), mean(dataout[(dataout$l==1),affected[m]]), 0.5, 0.5)
          res3  <- c((res1[1] - res2[1]), (res1[1] - res2[1]), (res1[1] - res2[1]), 0.5 , 0.5)
          a     <- c(res1, res2, res3)
          return(a)
        })
        table.who[((i-1)*length(affected)*15+(m-1)*15+1):((i-1)*length(affected)*15+(m)*15),l]   <- a 
        # table.who: first characteristic fills rows 1 to 15 and column 1 (each ML method gets a column), with rows 1 to 5 for high effect, 6 to 10 low effect, 11 to 15 difference..
        # Next outcome fills 16 to 30.
        # Repeat this for each characteristic. We have 14 characteristics. 14 x 30 x 2 elements when vectorized = 840.
        
      }
    }
  }
  res <- c(as.vector(results_test), as.vector(results), as.vector(results_het), as.vector(results_group), as.vector(bestML), as.vector(table.who))
  dim(res)
  print(t)
  r <- data.frame(res)
}
ptm

write.csv(r,(paste(data.path,"/hh_ml_output.csv",sep=""))) # save it to a csv
# bestML had a column for each ML method, with two rows per outcome, one for each goodness of fit measure. Now it is vectorized.
results_test  <- array(c(as.matrix(r[1:(15*length(Y)*length(methods)),])), c(15*length(Y),length(methods), sim)) # After putting everything from a single run through in one column, we start to break it up again.
# note that the columns are the sims, and then there are blocks of rows in r for pieces of output. 
# results_test takes rows [1:(15*length(Y)*length(methods)),] or r and puts it into arrays with 15 x # of outcomes rows and # of columns = # of MLmethods, with a separate array for each sim.
# The next line extracts columns 61 to 80 from r and puts them in a matrix with 5 x number of outcome rows and number of methods columns, or 10 x 2, and there is a separate matrix for each sim (so 100 10 x 2 matrices)
results       <- array(c(as.matrix(r[((15*length(Y)*length(methods))+1):((15+5)*length(Y)*length(methods)),])), c(5*length(Y),length(methods), sim))
results_het   <- array(c(as.matrix(r[(((20)*length(Y)*length(methods))+1):((20+5)*length(Y)*length(methods)),])), c(5*length(Y),length(methods), sim)) # rows 41 to 50.
results_group <- array(c(as.matrix(r[(((25)*length(Y)*length(methods))+1):((25+9)*length(Y)*length(methods)),])), c(9*length(Y),length(methods), sim))
bestML        <- array(c(as.matrix(r[(((34)*length(Y)*length(methods))+1):((34+2)*length(Y)*length(methods)),])), c(2*length(Y),length(methods), sim))
table.who     <- array(c(as.matrix(r[(((36)*length(Y)*length(methods))+1):((36+length(affected)*15)*length(Y)*length(methods)),])), c(length(affected)*15*length(Y),length(methods), sim))

# now bestML has two rows per outcome and # of columns equal to # of ML methods for each sim.
results_all       <- t(sapply(seq(1:nrow(results[,,1])), function(x) colMedians(t(results[x,,])))) 
# going from 1 to the number of rows in results, take the column-wide medians for each row in results. 
# Rest of code below does the same for every other piece.
results_het_all   <- t(sapply(seq(1:nrow(results_het[,,1])), function(x) colMedians(t(results_het[x,,]))))
test_all          <- t(sapply(seq(1:nrow(results_test[,,1])), function(x) colMedians(t(results_test[x,,]))))
table.who_all     <- t(sapply(seq(1:nrow(table.who[,,1])), function(x) colMedians(t(table.who[x,,]))))
group_all         <- t(sapply(seq(1:nrow(results_group[,,1])), function(x) colMedians(t(results_group[x,,]))))
bestML_all        <- t(sapply(seq(1:nrow(bestML[,,1])), function(x) colMedians(t(bestML[x,,]))))
#bestML_all: Columns are goodness of fit measures.
best1 <- order(-bestML_all[1,])[1:2] #this gives the index of the ML method with best goodness of fit in first row (the first outcome, first gof measure) followed by second best g.o.f. 
best2 <- order(-bestML_all[2,])[1:2] #does the same for the second row (first outcome, second goodness of fit

if(best1[1]!=best2[1]){ best <- c(best1[1],best2[1])} # if first outcome and second outcome don't have same best ML method in terms of g.o.f., best has column index of best ML for outcome 1 followed by best ML for outcome 2
if(best1[1]==best2[1]){ best <- c(best2[1],best2[2])} # if first outcome and second outcome have same best ML method in terms of g.o.f., 

table.who_all2   <- matrix(0,length(affected)*12*length(Y), length(methods)) # Matrix of zeroes with rows equal to 12 x number of variables in 'affected' x number of outcomes, with number of columns equal to number of ML methods. 12 because we have high, low, and difference.
results_all2     <- matrix(NA,4*length(Y), length(methods)) # Matrix of missing values with 4 x number of outcomes rows and columns equal to number of methods. 
results_het_all2 <- matrix(NA,4*length(Y), length(methods))
test_all2        <- matrix(NA,12*length(Y), length(methods))

l <- 1

for(i in seq(1, nrow(table.who_all), 5)){ # from 1 to number of rows in table.who.all in increments of 5
  # First time through: rows 1 to 3 and all columns of table.who.all2 equal to rows 1 to 3 of all columns of table.who.all.
  # next time throuhg: rows 5 to 7...equal to rows 6 to 8....
  table.who_all2[l:(l+2),]  <- table.who_all[i:(i+2),]
  # First time through: row 4 all columsn equal to minimum of 1 and 4 times the minimum of the third and fourth rows of table.who_all. THis must be the p-value.
  table.who_all2[l+3,]      <- sapply(seq(1:length(methods)), function(x) min(1,4*min(table.who_all[i+3,x], table.who_all[i+4,x])))
  
  if(l<nrow(results_all2)){
    results_all2[l:(l+2),]       <- results_all[i:(i+2),]
    results_het_all2[l:(l+2),]   <- results_het_all[i:(i+2),]
    
    results_all2[l+3,]     <- sapply(seq(1:length(methods)), function(x) min(1,4*min(results_all[i+3,x], results_all[i+4,x])))
    results_het_all2[l+3,] <- sapply(seq(1:length(methods)), function(x) min(1,4*min(results_het_all[i+3,x], results_het_all[i+4,x])))
  }
  if(l<nrow(test_all2)){
    
    test_all2[l:(l+2),]          <- test_all[i:(i+2),]
    test_all2[l+3,]              <- sapply(seq(1:length(methods)), function(x) min(1,4*min(test_all[i+3,x], test_all[i+4,x])))
  }
  
  l <- l+4
}
write.csv(results_all2,(paste(data.path,"/hh_results_all2.csv",sep=""))) # save it to a csv
write.csv(results_het_all2,(paste(data.path,"/hh_results_het_all2.csv",sep=""))) # save it to a csv
write.csv(test_all2,(paste(data.path,"/hh_test_all2.csv",sep=""))) # save it to a csv
write.csv(bestML_all,(paste(data.path,"/hh_bestML_all.csv",sep=""))) # save it to a csv
write.csv(table.who_all2,(paste(data.path,"/hh_table.who_all2.csv",sep=""))) # save it to a csv

results_all     <- round(results_all2, digits = 3)
results_het_all <- round(results_het_all2, digits = 3)
test_all        <- round(test_all2, digits = 3)
bestML_all      <- format(round(bestML_all, pmax(0,4-nchar(floor(abs(bestML_all))))), nsmall= pmax(0,4-nchar(floor(abs(bestML_all)))))
table.who_all   <- round(table.who_all2, digits = 3)

results_test2   <- matrix(0,9*length(Y), length(methods))
result2         <- matrix(0,3*length(Y), length(methods))
result_het2     <- matrix(0,3*length(Y), length(methods))
table.who_all2  <- matrix(0,9*length(Y)*length(affected), length(methods))

seq3 <- seq(1, nrow(table.who_all), 4)
l    <- 1
for(i in seq(1, nrow(table.who_all2), 3)){
  
  k <- seq3[l]
  
  if(i<nrow(result2)){
    result2[i,]       <- format(round(results_all[k,], pmax(0,4-nchar(floor(abs(results_all[k,]))))), nsmall= pmax(0,4-nchar(floor(abs(results_all[k,])))))
    result2[i+1,]     <- sapply(seq(1:ncol(results_all)), function(x) paste("(", format(round(results_all[k+1,x],pmax(0,4-nchar(floor(abs(results_all[k+1,x]))))), nsmall=pmax(0,4-nchar(floor(abs(results_all[k+1,x]))))), ",", format(round(results_all[k+2,x],pmax(0,4-nchar(floor(abs(results_all[k+2,x]))))) , nsmall=pmax(0,4-nchar(floor(abs(results_all[k+2,x]))))), ")", sep=""))
    result2[i+2,]     <- paste("[", format(results_all[k+3,], nsmall = pmax(0,4-nchar(floor(abs(results_all[k+3,]))))), "]", sep="")
    
    result_het2[i,]   <- format(round(results_het_all[k,],max(0,4-nchar(floor(abs(results_het_all[k,]))))) , nsmall=pmax(0,4-nchar(floor(results_het_all[k,]))))
    result_het2[i+1,] <- sapply(seq(1:ncol(results_het_all)), function(x) paste("(", format(round(results_het_all[k+1,x], pmax(0,4-nchar(floor(abs(results_het_all[k+1,x]))))) , nsmall=pmax(0,4-nchar(floor(abs(results_het_all[k+1,x]))))), ",", format(round(results_het_all[k+2,x],pmax(0,4-nchar(floor(abs(results_het_all[k+2,x]))))) , nsmall=pmax(0,4-nchar(floor(abs(results_het_all[k+2,x]))))), ")", sep=""))
    result_het2[i+2,] <- paste("[", format(results_het_all[k+3,], nsmall=max(0,4-nchar(floor(abs(results_het_all[k+3,]))))), "]", sep="")
  }
  
  if(i<nrow(results_test2)){
    results_test2[i,]    <- format(round(test_all[k,],pmax(0,4-nchar(floor(abs(test_all[k,]))))) , nsmall=pmax(0,4-nchar(floor(test_all[k,]))))
    results_test2[i+1,]  <- sapply(seq(1:ncol(test_all)), function(x) paste("(", format(round(test_all[k+1,x], pmax(0,4-nchar(floor(abs(test_all[k+1,x]))))),nsmall=pmax(0,4-nchar(floor(abs(test_all[k+1,x]))))), ",", format(round(test_all[k+2,x],pmax(0,4-nchar(abs(floor(test_all[k+2,x]))))),  nsmall=pmax(0,4-nchar(floor(abs(test_all[k+2,x]))))), ")", sep=""))
    results_test2[i+2,]  <- paste("[", format(test_all[k+3,], nsmall=pmax(0,4-nchar(floor(abs(test_all[k+3,]))))), "]", sep="")
  }
  table.who_all2[i,]       <- format(round(table.who_all[k,],pmax(0,4-nchar(floor(abs(table.who_all[k,]))))) ,nsmall=pmax(0,4-nchar(floor(abs(table.who_all[k,])))))
  table.who_all2[i+1,]     <- sapply(seq(1:ncol(table.who_all)), function(x) paste("(", format(round(table.who_all[k+1,x], pmax(0,4-nchar(floor(abs(table.who_all[k+1,x]))))), nsmall=pmax(0,4-nchar(floor(abs(table.who_all[k+1,x]))))), ",", format(round(table.who_all[k+2,x],pmax(0,4-nchar(floor(abs(table.who_all[k+2,x]))))) , nsmall=pmax(0,4-nchar(floor(abs(table.who_all[k+2,x]))))), ")", sep=""))
  if(i%%9==7){  table.who_all2[i+2,]     <- paste("[", format(table.who_all[k+3,], nsmall=pmax(0,4-nchar(floor(abs(table.who_all[k+3,]))))), "]", sep="") }
  if(i%%9!=7){  table.who_all2[i+2,]     <- "-" }
  
  
  l <- l+1
}


CLAN_final         <- matrix(NA, length(Y)*(length(affected)*3+1), length(best)*3)
GATES_final        <- matrix(NA, length(Y)*3, length(best)*3)
BLP_final          <- matrix(NA, length(Y)*3, length(best)*2)
BEST_final         <- bestML_all

rownames_CLAN      <- matrix(NA, nrow(CLAN_final),1)
rownames_GATES     <- matrix(NA, nrow(GATES_final),1)
rownames_BEST      <- matrix(NA, nrow(bestML_all),1)

a  <- 1
b  <- 1
c  <- 1
c2 <- 1

for(l in 1:length(Y)){
  
  rownames_CLAN[a] <- names[l]
  
  a <- a+1
  
  for(i in 1:length(affected)){
    for(j in 1:length(best)){
      
      k <- best[j] # sets k equal to first element in best. If first 2 outcomes both had same ML method for best g.o.f., then k is column index of that method. If they didn't, it is best method for second outcome.
      
      CLAN_final[(a):(a+2),((j-1)*3+1):(j*3)] <- matrix(table.who_all2[(b):(b+8),k], 3, 3) # columns1 to 3 have method that is best for both if there is one. Otherwise, the columns just go in order.
      # first time through: CLAN rows 2 through 4 and columns 1 through 3 are replaced with 

      if(i==1){
        GATES_final[(c):(c+2),((j-1)*3+1):(j*3)] <- matrix(results_test2[(c2):(c2+8),k], 3, 3)
        rownames_GATES[c]   <- names[l]
        BLP_final[(c):(c+2),((j-1)*2+1):(j*2)] <- cbind(result2[(c):(c+2),j], result_het2[(c):(c+2),k])
      }
      
      rownames_CLAN[a]   <- names_affected[i]
    }
    a <- a+3
    b <- b+9
  }
  c  <- c+3
  c2 <- c2+9
  
  rownames_BEST[((l-1)*2+1):((l-1)*2+2)] <- c(names[l], names[l])
  
}

rownames(CLAN_final)   <- rownames_CLAN
rownames(GATES_final)  <- rownames_GATES
rownames(BLP_final)    <- rownames_GATES
rownames(BEST_final)   <- rownames_BEST

colnames(CLAN_final)   <- rep(c("Most Affected", 	"Least Affected",	"Difference"), length(best))
colnames(GATES_final)  <- rep(c("Most Affected", 	"Least Affected",	"Difference"), length(best))
colnames(BLP_final)    <- rep(c("ATE", 	"HET"), length(best))
colnames(BEST_final)   <- method_names

print(xtable(cbind(rownames(BLP_final),BLP_final)), include.rownames=FALSE,file=paste(name,"hh_BLP","-",output_name,".txt",sep=""), digits=3)
print(xtable(cbind(rownames(GATES_final),GATES_final)), include.rownames=FALSE,file=paste(name,"hh_GATES","-",output_name,".txt",sep=""), digits=3)
print(xtable(cbind(rownames(BEST_final),BEST_final)), include.rownames=FALSE,file=paste(name,"hh_BEST","-",output_name,".txt",sep=""), digits=3)
print(xtable(cbind(rownames(CLAN_final),CLAN_final)), include.rownames=FALSE,file=paste(name,"hh_CLAN","-",output_name,".txt",sep=""), digits=3)

write.csv(CLAN_final,(paste(data.path,"/hh_CLAN_final.csv",sep=""))) # save it to a csv
write.csv(GATES_final,(paste(data.path,"/hh_GATES_final.csv",sep=""))) # save it to a csv
write.csv(BLP_final,(paste(data.path,"/hh_BLP_final.csv",sep=""))) # save it to a csv
write.csv(BEST_final,(paste(data.path,"/hh_BEST_final.csv",sep=""))) # save it to a csv

