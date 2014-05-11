rm(list=ls())

##########################################################
# Options
##########################################################
opts.my_seed <- 134
opts.scale <- F
opts.binning <- F
opts.pc <- 2 #0,2,3,5
##########################################################
# Supervised Learning using Naive Bayes algorithm.
##########################################################

library(e1071)
library(class)
library(gmodels)

library(caret)
require(foreign, quietly = TRUE)

source("~/src/R/functions/998.support.functions.R")
source("~/src/R/functions/dir_list.R")

## Equal Width Interval Binning k=2 ##
convert_counts <- function(x) {
  return (factor(ifelse(x > ((max(x) - min(x))/2), 1, 0),
                 levels = c(0, 1), labels = c("A", "B")))
}
##

set.seed(opts.my_seed)

flist <- dir.list("~/Documenti/new_thesis/NASA-SoftwareDefectDataSets/MDP/D'/")

for (fdataset in flist) {
  ds_name <- substr(basename(fdataset), 1, 3)
  print(paste("dataset:", ds_name, sep=' '))
  
  myDataset_o <- na.omit(read.arff(fdataset))
  
  # Try to be smart and get the independent variables names and the dependent variable name.
  input <- colnames(myDataset_o)[-ncol(myDataset_o)]
  target <- colnames(myDataset_o)[ncol(myDataset_o)]
  
  # Make a copy of the original data set.
  if (opts.pc == 0) {
    myDataset <- myDataset_o
  } else {
    myDataset <- myDataset_o[, -ncol(myDataset_o)]
  }
  
  nobs <- nrow(myDataset)
  
  if (opts.binning) {
    myDataset <- as.data.frame(lapply(myDataset_o[, input], convert_counts))
    myDataset[target] <- myDataset_o[,target]
  }
  
  myDataset.train.index <- sample(nrow(myDataset), 0.75 * nobs)
  myDataset.test.index <- sample(setdiff(seq_len(nrow(myDataset)), myDataset.train.index), 0.25 * nobs)
  myDataset.train <- myDataset[myDataset.train.index, ]
  myDataset.test <- myDataset[myDataset.test.index, ]
  
  myDataset.train.labels <- myDataset.train[,ncol(myDataset.train)]
  myDataset.test.labels <- myDataset.test[,ncol(myDataset.test)]
  
  if (opts.pc == 0) {
    # Evaluate Naive Bayes with a 10-fold cross-validation
    nbGrid <- expand.grid(usekernel=T, fL=c(0:3))
    fit <- train(myDataset.train[,input], myDataset.train$Defective, "nb",
                 trControl = trainControl(method="cv", number=10),
                 tuneGrid = nbGrid,
    )
    pred <- predict(fit$finalModel, myDataset.test)#, type="raw")
    CrossTable(pred$class, myDataset.test.labels,
               prop.chisq=F, prop.t=F, dnn=c('pred', 'actual'))
  } else {
    pcs.D1 <- svd.pc(as.matrix(myDataset.train), 
                     as.matrix(myDataset.test),
                     k = opts.pc, plot = FALSE)
    
    myDataset.train.labels <- myDataset_o[myDataset.train.index,ncol(myDataset_o)]
    myDataset.test.labels <- myDataset_o[myDataset.test.index,ncol(myDataset_o)]
    
    nbGrid <- expand.grid(usekernel=T, fL=c(0:3))
    fit <- train(pcs.D1$scores.train[,1:opts.pc], myDataset.train.labels, "nb",
                 trControl = trainControl(method="cv", number=10),
                 tuneGrid = nbGrid)
    pred <- predict(fit$finalModel, pcs.D1$scores.test)#, type="raw")
    CrossTable(pred$class, myDataset.test.labels,
               prop.chisq=F, prop.t=F, dnn=c('pred', 'actual'))
  }
}