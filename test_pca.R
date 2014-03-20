rm(list=ls())

#source('~/Dropbox/exchange/sasanelli/elab/999.support.functions.R', chdir = TRUE)
source("/home/fmarella/src/R/functions/999.support.functions.R")

library(foreign)
#myDataset <- read.arff("~/Google Drive/papers.new/frozen/2013boffoli/data/MDP/D1/CM1.arff")
myDataset <- read.arff("file:///home/fmarella/Documenti/materiale_tesi/NASA-SoftwareDefectDataSets/MDP/D'/CM1.arff")

feature.vector <- myDataset[, -38]
nobs <- nrow(myDataset)
myDataset.train.index <- sample(nrow(feature.vector), 0.75 * nobs)
myDataset.test.index <- sample(setdiff(seq_len(nrow(feature.vector)), myDataset.train.index), 0.25 * nobs)
myDataset.train <- feature.vector[myDataset.train.index, ]
myDataset.test <- feature.vector[myDataset.test.index, ]

svd.pc(myDataset.train, as.matrix(myDataset.test), k=10, plot=FALSE)
