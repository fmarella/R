#####################################################################################
#####################################################################################
##### FUNZIONE PER L'ESTRAZIONE DEGLI SCORE DELLE COMPONENTI PRINCIPALI 
##### E DEI RELATIVI LOADINGS 
svd.pc <- function(train, test, k, trainset=deparse(substitute(train)), center=TRUE, scale=FALSE, plot=TRUE, save.plot=FALSE, fig.path=figures.path){
##	require(subselect)
	trainset <- as.character(trainset)
	if(!is.matrix(train))
## Tipicamente train è un dataframe, e viene forzata una coercizione
## ad un oggetto di classe matrix
	{
		train  <- as.matrix(train)
	}
## Calcola il rango di data.matrix. Se il rango di colonna è pieno.
## mon esiste ridondanza perfetta nel senso che nessuna metrica è ottenbile
## come perfetta combilanzione lineare di altre
	rank.train  <- length(which(round(svd(train)$d,4)!=0))
## print(rank.X.train.uncentered)
## Per default centra la matrice dei dati
	if(center)
	{
		train  <- scale(train, center=T, scale=F)
		centers <- attr(train, "scaled:center")
			if(center & scale)
			{
## Eventualmente la riscala: equivalente a calcolare le componenti principali
## sulla matrice campionaria di correlazione
				train  <- scale(train, center=T, scale=T)
				scales <- attr(train, "scaled:scale")
			}
	}
## Calcola gli score delle componenenti principali mediante la SVD
	Z <- svd(train)$u[,1:rank.train] %*% diag(svd(train)$d[1:rank.train])
	colnames(Z) <- paste("PC", 1:rank.train, sep="")
	rownames(Z) <- rownames(train)
##	Z <- round(cov(Z),4)
	svd.pcs <- list(
		rank = rank.train, 
		scores.train = Z, 
		singular.values = svd(train)$d[1:rank.train],
		variances = diag(cov(Z)),
		loadings = svd(train)$v
	)
	## colnames(svd.pcs$loadings) <- paste("PC", 1:rank.train, sep="")
	## rownames(svd.pcs$loadings) <- colnames(train)
## Utilizziamo lo stesso sistema di coordinate che abbiamo trovato per
## il training set, proiettando i dati contenuti in 'test'
## secondo l'equazione Z_test = X_test %*% V_train, dove 'X_test' è 
## la matrice di dimensione che contiene le metriche nel test set 
## V è la matrice dei loadings di dimensione (19x19) ottenuta da 'svd(train)$v'
## mediante la SVD nel training set utilizzato
## ATTENZIONE: BISOGNA PROIETTARE RISPETTO ALLO STESSO SISTEMA DI COORDINATE
## USATO PER LE PC NEL TRAINING SET
	if(center)
	{
		test  <- scale(test, center=centers, scale=F) 
	}
	if(center & scale)
	{
		test  <- scale(test, center=F, scale=scales)
	}
	svd.pcs$scores.test = (test %*% svd.pcs$loadings)[,1:rank.train]
	colnames(svd.pcs$scores.test) <- paste("PC", 1:rank.train, sep="")
## Calcola la varianza cumulata in percentuale
	lenght.total <- sum(diag(cov(Z)))
	var.percent <- cumsum(diag(cov(Z)))/lenght.total
	x <- var.percent[1:k]
## Aggiunge nell'oggetto che viene restituito il numero di componenti principali 
## che spiegano almeno il 95% e il 99% della varianza complessiva
	svd.pcs$npcs.0.95 <- min(which(var.percent > 0.95))	
	svd.pcs$npcs.0.99 <- min(which(var.percent > 0.99))	
## Passa i risultati della funzione rm.coef() nel package 'subselect'
##	rm.temp <- numeric(length=dim(train)[2])
##	rv.temp <- numeric(length=dim(train)[2])
##	for(i in 1:(dim(train)[2]))
##	{
##		rm.temp[i] <- rm.coef(cov(train), indices=1:i)
##		rv.temp[i] <- rv.coef(cov(train), indices=1:i)
##	}
##	svd.pcs$rm.coefs = rm.temp
##	svd.pcs$rv.coefs = rv.temp
	if(plot)
	{
## Salva la figura qualora save.plot=TRUE
		if(save.plot)
		{
			if(center)
			{
				filename.postscript <- paste(fig.path, "metric.pc.centered.", trainset, ".kubios.lambda", lambda, ".", artefact, ".eps", sep = "")
				if(center & scale)
				{
					filename.postscript <- paste(fig.path, "metric.pc.centered.scaled.", trainset, ".kubios.lambda", lambda, ".", artefact, ".eps", sep = "")
				}
			}					
			postscript(file=filename.postscript, width=12, height=12, paper="special", pointsize=22)	
		}
## Prepara lo splitting degli schermi							
		split.screen(c(2,1))
		split.screen(c(1,2), screen=1)		
## Plotta le varianze
		screen(3)
			par(mar = c(5, 4, 1.2, 0) + 0.2, las=3)
			barplot(diag(cov(Z))[1:k])
			title(ylab="Variance") 
## Plotta le varianze cumulate
		screen(4)
			par(mar = c(5, 4, 1, 0.5) + 0.1)
			plot(x*100, type="b", pch=21, col="black", yaxt="n", xaxt="n", lty=3, lwd=2.5, xlab="", ylab="")
			axis(1, at=1:k, labels=1:k, col.axis="black", las=1)
			mod.ylabels <- round(min(var.percent)*100,2) %% 5
			at.ylabels <- seq(round(min(var.percent)*100,2)-mod.ylabels, 100, by=5)
			ylabels <- paste(at.ylabels, "%", sep="")	
			axis(2, at=at.ylabels, labels=ylabels, col.axis="black", las=1)
			abline(h=95, lwd=3, col="darkgray")
			abline(h=99, lwd=3, col="lightgray")
			abline(h=1)
			title(xlab="Principal component", ylab="Cumulated variance (%)")
			legend("bottomright", inset=0.01, legend=c("95%", "99%"), lwd=c(6,6), col=c("darkgray", "lightgray"), cex=0.7)
## Plotta i loadings delle componenti che spiegano almeno il 99% della variabilità complessiva
		screen(2)
			npcs <- min(which(var.percent > 0.99))	
			par(mar = c(6.5, 4, 0.2, 0.5) + 0.1, las=2)
			matplot(svd(train)$v[,1:npcs], type="l", lty=rep(1,npcs), lwd=rep(3.5, npcs), col=gray.colors(n=npcs, end=0.8), xlab="", ylab="Loadings", xaxt="n")
			lines(abline(h=0), lwd=1.5)
			axis(1, at=1:(dim(train )[2]), labels=colnames(train ))
			legend("bottomright", inset=0.01, legend=paste("PC", 1:npcs, sep=""), lwd=rep(6,npcs), col=gray.colors(n=npcs, end=0.8), cex=0.7)
		if(save.plot)
		{
			graphics.off()
		}
	}
	return(svd.pcs)
}

