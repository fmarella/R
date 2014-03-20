
source("./functions/999.support.functions.R")

# matrice istanze x attributi
myDataset <- matrix(c(
  1,0,0,0,2,
  0,0,3,0,0,
  0,0,4,0,8,
  0,4,0,0,0,
  2,1,6,4,0), byrow=T, ncol=5)

require(foreign, quietly = TRUE)
myDataset <- read.arff("file:///home/fmarella/Documenti/materiale_tesi/NASA-SoftwareDefectDataSets/MDP/D'/CM1.arff")

set.seed(12345)

nobs <- nrow(myDataset)

myDataset.train <- sample(nrow(myDataset), 0.75 * nobs)
myDataset.test <- sample(setdiff(seq_len(nrow(myDataset)), myDataset.train), 0.25 * nobs)

svd.pc(myDataset.train, myDataset.test, 3)
