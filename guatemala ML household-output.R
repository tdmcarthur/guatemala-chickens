###########################
# Code to output the results of "guatemala ML household.r" to csv and tex
###########################

# To use this code, run "guatemala ML household.r" up to the point that the main estimation loop finishes,
# which is the loop that begins this way:
# r <- foreach(t = 1:sim, .combine='cbind', .inorder=FALSE, .packages=vec.pac) %dopar% { 
# (This is approximately line 405)
# After the loop finishes, run this script.
# Most of this script is copied from the second section of "guatemala ML household.r"

# Alternatively, load in an R workspace that saved the workspace at the completion of the loop


vec.pac= c("foreign", "quantreg", "gbm", "glmnet",
           "MASS", "rpart", "doParallel", "sandwich", "randomForest",
           "nnet", "matrixStats", "xtable", "readstata13", "car", "lfe", "doParallel",
           "caret", "foreach", "multcomp","cowplot", "iterators", "tcltk", "future", "dplyr")
lapply(vec.pac, require, character.only = TRUE)
library("xtable")

# load("~/Dropbox (UFL)/guatemala-chickens/ML-household-results-after-ptm.Rdata")
# Here you would load the workspace from file

output.path <- ""
# MUST: Put the output path here


display.p.val <- TRUE
# Determine whether the p values will be displayed
add.space.to.CI.comma <- TRUE
# Should a space be added after the comma in the confidence interval?
ML.methods.to.separate.files <- TRUE
# Put the tables for the machine learning techniques into separate tex
# files rather than combined into one.
# NOTE: Currently this only works when using exactly two ML methods.
# With fewer or greater than 2 methods, the script will fail



# BEGIN code from "guatemala ML household.r"
###########################



write.csv(r,(paste(output.path,"/hh_ml_output.csv",sep=""))) # save it to a csv
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
write.csv(results_all2,(paste(output.path,"/hh_results_all2.csv",sep=""))) # save it to a csv
write.csv(results_het_all2,(paste(output.path,"/hh_results_het_all2.csv",sep=""))) # save it to a csv
write.csv(test_all2,(paste(output.path,"/hh_test_all2.csv",sep=""))) # save it to a csv
write.csv(bestML_all,(paste(output.path,"/hh_bestML_all.csv",sep=""))) # save it to a csv
write.csv(table.who_all2,(paste(output.path,"/hh_table.who_all2.csv",sep=""))) # save it to a csv

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




# BEGIN TM's code edits

if (!display.p.val) {
  GATES_final <- GATES_final[!grepl("[", GATES_final[, 1], fixed = TRUE), ]
  BLP_final <- BLP_final[!grepl("[", BLP_final[, 1], fixed = TRUE), ]
}

if (add.space.to.CI.comma) {
  GATES_final <- gsub(",", ", ", GATES_final)
  BLP_final <- gsub(",", ", ", BLP_final)
}


# END TM's code edits



print(xtable(cbind(rownames(BLP_final),BLP_final)), include.rownames=FALSE,file=paste(name,"hh_BLP","-",output_name,".txt",sep=""), digits=3)
print(xtable(cbind(rownames(GATES_final),GATES_final)), include.rownames=FALSE,file=paste(name,"hh_GATES","-",output_name,".txt",sep=""), digits=3)
print(xtable(cbind(rownames(BEST_final),BEST_final)), include.rownames=FALSE,file=paste(name,"hh_BEST","-",output_name,".txt",sep=""), digits=3)
print(xtable(cbind(rownames(CLAN_final),CLAN_final)), include.rownames=FALSE,file=paste(name,"hh_CLAN","-",output_name,".txt",sep=""), digits=3)

write.csv(CLAN_final,(paste(output.path,"/hh_CLAN_final.csv",sep=""))) # save it to a csv
write.csv(GATES_final,(paste(output.path,"/hh_GATES_final.csv",sep=""))) # save it to a csv
write.csv(BLP_final,(paste(output.path,"/hh_BLP_final.csv",sep=""))) # save it to a csv
write.csv(BEST_final,(paste(output.path,"/hh_BEST_final.csv",sep=""))) # save it to a csv




# BEGIN TM's code edits


BEST_final.w.rownames <- cbind(data.frame(Variable = rownames(BEST_final)), BEST_final)

print(xtable(BEST_final.w.rownames, 
             align = c("l", "l", rep("c", ncol(BEST_final.w.rownames) - 1))), 
      file = paste0(output.path, "/hh_BEST_final.tex"), 
      include.rownames = TRUE)



if (ML.methods.to.separate.files) {
  
  GATES_final.1 <- cbind(data.frame(Variable = rownames(GATES_final)), GATES_final[, 1:3])
  GATES_final.2 <- cbind(data.frame(Variable = rownames(GATES_final)), GATES_final[, 4:6])
  
  print(xtable(GATES_final.1, 
             align = c("l", "l", rep("c", ncol(GATES_final.1) - 1))),
        file = paste0(output.path, "/hh_GATES_final_", gsub(" ", "_", method_names[1]), ".tex"), 
        include.rownames = FALSE)
  
  print(xtable(GATES_final.2, 
             align = c("l", "l", rep("c", ncol(GATES_final.2) - 1))), 
        file = paste0(output.path, "/hh_GATES_final_", gsub(" ", "_", method_names[2]), ".tex"), 
        include.rownames = FALSE)
  
  BLP_final.1 <- cbind(data.frame(Variable = rownames(BLP_final)), BLP_final[, 1:2])
  BLP_final.2 <- cbind(data.frame(Variable = rownames(BLP_final)), BLP_final[, 3:4])
  
  print(xtable(BLP_final.1, 
             align = c("l", "l", rep("c", ncol(BLP_final.1) - 1))), 
        file = paste0(output.path, "/hh_BLP_final_", gsub(" ", "_", method_names[1]), ".tex"), 
        include.rownames = FALSE)
  
  print(xtable(BLP_final.2, 
             align = c("l", "l", rep("c", ncol(BLP_final.2) - 1))), 
        file = paste0(output.path, "/hh_BLP_final_", gsub(" ", "_", method_names[2]), ".tex"), 
        include.rownames = FALSE)
  
} else {
  
  GATES_final.w.rownames <- cbind(data.frame(Variable = rownames(GATES_final)), GATES_final)
  
  print(xtable(GATES_final.w.rownames, 
             align = c("l", "l", rep("c", ncol(GATES_final.w.rownames) - 1))),  
        file = paste0(output.path, "/hh_GATES_final.tex"), 
        include.rownames = FALSE)
  
  
  BLP_final.w.rownames <- cbind(data.frame(Variable = rownames(BLP_final)), GATES_final)
  
  print(xtable(BLP_final.w.rownames, 
             align = c("l", "l", rep("c", ncol(BLP_final.w.rownames) - 1))),  
        file = paste0(output.path, "/hh_GATES_final.tex"), 
        include.rownames = FALSE)
  
  
  
}


# END TM's code edits












