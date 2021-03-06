rm(list=ls())
##########################################################
# Options
##########################################################
opts.my_seed <- 1347
# opts.scale <- T
opts.normalize <- F
opts.svm_kernel <- "radial"
##########################################################

# Load libraries for data partitioning, confusion matrix.
library(caret)
library(e1071)
library(ROCR)
require(foreign, quietly = TRUE)
library(class)

set.seed(opts.my_seed)

source("~/src/R/functions/998.support.functions.R")

source("~/src/R/functions/dir_list.R")
flist <- dir.list("~/Documenti/new_thesis/NASA-SoftwareDefectDataSets/MDP/D'/")
#source("./R/functions/dir_list.R")
#flist <- dir.list("./NASA-SoftwareDefectDataSets/MDP/D'/")

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

print("feature set completo")

for (fdataset in flist) {
  ds_name <- substr(basename(fdataset), 1, 3)
  print(paste("dataset:", ds_name, sep=' '))
  
  myDataset_o <- na.omit(read.arff(fdataset))
  
  # 
  input <- colnames(myDataset_o)[-ncol(myDataset_o)]
  target <- colnames(myDataset_o)[ncol(myDataset_o)]
  
  myDataset <- myDataset_o
  nobs <- nrow(myDataset)
  print(paste("istanze:", nobs, sep=" "))
  
  myDataset.train.index <- sample(nrow(myDataset), 0.75 * nobs)
  myDataset.test.index <- sample(setdiff(seq_len(nrow(myDataset)), myDataset.train.index), 0.25 * nobs)
  myDataset.train <- myDataset[myDataset.train.index, c(input, target)]
  myDataset.test <- myDataset[myDataset.test.index, c(input, target)]
  
  #myDataset.train.labels <- myDataset.train[,ncol(myDataset.train)]
  #myDataset.test.labels <- myDataset.test[,ncol(myDataset.test)]
  myDataset.train.labels <- myDataset_o[myDataset.train.index, ncol(myDataset_o)]
  myDataset.test.labels <- myDataset_o[myDataset.test.index, ncol(myDataset_o)]  

  ## Data transformation - normalization and scaling of numeric attributes
  #   if (opts.scale) {
  #     myDataset <- lapply(myDataset_o[, input], scale)
  #     myDataset[target] <- myDataset_o[,target]
  #   }
  
  if (opts.normalize) {
    myDataset <- as.data.frame(lapply(myDataset_o[, input], normalize))
    myDataset[target] <- myDataset_o[,target]
  }
  
  tryCatch({
    ## kNN
    tuned.knn <- tune.knn(x=myDataset[,input], y=myDataset[,target],
                          k = 1:25,
                          tunecontrol = tune.control(sampling = "cross", cross = 10))
    print(paste("knn Accuratezza: ", round((1 - tuned.knn$best.performance) * 100, 3), " %", sep=''))
  }, warning = function(war) {
    print(paste("Warning:", war))
  }, error = function(err) {
    print(paste("Caught:", err))
  }, finally = {
  })    
    
  tryCatch({
    ## SVM
    tuned.svm <- tune.svm(Defective~., kernel = opts.svm_kernel, data = myDataset.train,
                          gamma = 10^(-6:-1), cost = 10^(1:2),
                          tunecontrol = tune.control(sampling = "cross", cross = 10))
    
    # Get the accuracy and other measures.
    final.pred <- predict(tuned.svm$best.model, myDataset.test[,input])
    conf.matrix <- table(true = myDataset.test.labels, pred = final.pred)
    
    print(paste("svm Accuratezza:", round(classAgreement(conf.matrix)$diag*100, 3), "%", sep=" "))
  }, warning = function(war) {
    print(paste("Warning:", war))
  }, error = function(err) {
    print(paste("Caught:", err))
  }, finally = {
  })
}
