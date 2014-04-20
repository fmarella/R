rm(list = ls())

library(foreign)
library(hash)
require(Hmisc, quietly=T)
source("~/src/R/functions/dir_list.R")

flist <- dir.list("~/Documenti/new_thesis/NASA-SoftwareDefectDataSets/MDP/D'/")

sanitizeLatexS <- function(str) {
  gsub('([#$%&~_\\^\\\\{}])', '\\\\\\\\\\1', str, perl = TRUE);
}

for (fdataset in flist) {
  
  ds_name <- substr(basename(fdataset), 1, 3)
  # print(paste('Analisi delle CP: dataset ', ds_name, sep=''))
  
  myDataset <- read.arff(fdataset)
  # summary(myDataset)

  #colnames(myDataset) <- map(colnames(myDataset))
  df <- data.frame(Metrica=colnames(myDataset), Etichetta=paste("A", 1:ncol(myDataset), sep=''))
  
#   print(which(amap$x == "LOC_TOTAL"))
#   print(which(amap$x == "Defective"))

  a <- latex(df, where="h", label=paste("tab:map", ds_name, sep=""), append=T,
             caption=paste("Dataset ", ds_name, sep=""),
             rowlabel="",
             file=paste("table.mapped.all.tex", sep=''))

}
