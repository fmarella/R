rm(list = ls())

set.seed(123)

library(class)
library(foreign)
source("~/src/R/functions/998.support.functions.R")
source("~/src/R/functions/dir_list.R")

dir.create("/tmp/PCA")

flist <- dir.list("~/Documenti/new_thesis/NASA-SoftwareDefectDataSets/MDP/D'/")

for (fdataset in flist) {
    
    ds_name <- substr(basename(fdataset), 1, 3)
    # print(paste('Analisi delle CP: dataset ', ds_name, sep=''))
    
    myDataset <- read.arff(fdataset)
    # summary(myDataset)
    myDataset_o <- myDataset
    
    feature.vector <- myDataset[, -ncol(myDataset)]
    nobs <- nrow(myDataset)
    myDataset.train.index <- sample(nrow(feature.vector), 0.75 * nobs)
    myDataset.test.index <- sample(setdiff(seq_len(nrow(feature.vector)), myDataset.train.index), 
        0.25 * nobs)
    myDataset.train <- as.matrix(feature.vector[myDataset.train.index, ])
    myDataset.test <- as.matrix(feature.vector[myDataset.test.index, ])
    
    myDataset.train.labels <- myDataset_o[myDataset.train.index, ncol(myDataset_o)]
    myDataset.test.labels <- myDataset_o[myDataset.test.index, ncol(myDataset_o)]
    
    pcs.D1 <- svd.pc(myDataset.train, myDataset.test, k = 5, plot = FALSE)
    ## tutti gli oggetti sono qui dentro
    str(pcs.D1)
    
    ## scores delle PCs sul trainset (utilizzarne solo la prima, che spiega già il
    ## 100%, oppure le prime tre o le prime cinque per confrontare)
    pcs.D1$scores.train
    
    #myDataset["Defective"] <- myDataset_o[,"Defective"]
    
#     knn(pcs.D1$scores.train[,1:3], test=pcs.D1$scores.test[,1:3], cl=myDataset.train.labels,
#         #myDataset.train[,ncol(myDataset.train)],
#         k=21)
#     library(kernlab)
#     m <- ksvm(pcs.D1$scores.train[,1:3], data=pcs.D1$scores.train[,1:3],
#               kernel="rbfdot", C=1)
#     p <- predict(m, pcs.D1$scores.test[,1:3], type="response")
#     conf.matrix <- table(pred=p, true=myDataset.test.labels)
#     print(paste("svm Accuratezza:", round(classAgreement(conf.matrix)$diag*100, 3), "%", sep=" "))


    ## scores del test set proiettati nello stesso sistema di riferimento del training
    ## set. E' rispetto a queste che bisogna valutare l'accuratezza se usiamo le
    ## componenti principali (stesso numero di colonne, ovviamente, usato nel train
    ## set)
    pcs.D1$scores.test
    ## numero di componenti che spiegano almeno il 95% della varianza complessiva
    pcs.D1$npcs.0.95
    ## numero di componenti che spiegano almeno il 99% della varianza complessiva
    pcs.D1$npcs.0.99
    ## in questo caso è sempre 1 (basta in teoria una sola componente), data la
    ## fortissima ridondanza il grafico va un po' sistemato, i nomi originali delle
    ## variabili fanno casino inoltre il fatto che dopo già una componente siamo al
    ## 100% pone problemi nelle barre orizzontali che dovrebbero apparire nel grafico
    ## in altro a dx
    
    svd.pc(myDataset.train, myDataset.test, k = 5, plot = TRUE, unique.id = ds_name, 
        save.plot = TRUE, fig.path = "/tmp/PCA/")
    ## dai loadings in basso, è evidente che nella prima componente principale pesa
    ## sostanzialmente una sola metrica (quello con il picco più alto) la correlazione
    ## tra le metriche è davvero molto forte questa è la matrice di correlazione sui
    ## dati originali nel trainset non ruotati nelle componenti principali

    opath <- getwd()
    setwd("/tmp/")

    library(corrplot)
    y <- myDataset.train
    colnames(y) <- c(paste("A", 1:ncol(y), sep=''))
    png(filename=paste("corrplot.prepca.", ds_name, ".png", sep=''), width=380, height=380, title="foobar")
    corrplot(cor(y), method = "circle", type = "upper", tl.cex = 0.8, mar=c(1,1,1,1),
             title=paste("Correlazione dataset", ds_name, "originale", sep=' '))
    dev.off()
    ## nelle coordinate ruotate, ossia le PCs, le ridondanze sparisocono ovviamente
    ## del tutto. Stupefacente, vero?
    png(filename=paste("corrplot.postpca.", ds_name, ".png", sep=''), width=380, height=380)
    corrplot(cor(pcs.D1$scores.train), title=paste("Correlazione dataset", ds_name, "con PC", sep=' '),
             method = "circle", type = "upper", tl.cex=0.80, mar=c(1,1,1,1)) #, )#, tl.pos=45)
    ## Questo vuol dire che spesso vengono spesi un sacco di soldi e perso tempo
    ## inutile il grafico con i loadings è interessante, perchè ci permette di capire
    ## progetto per progetto quale metrica conta davvero
    dev.off()
    setwd(opath)
}