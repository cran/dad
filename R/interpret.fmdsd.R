interpret.fmdsd <-
  function(x, nscore=1, moment=c("mean", "sd", "var", "cov", "cor", "skewness", "kurtosis"), ...)
  { 
    if (moment[1] == "all") 
      stop("Since dad-4, moment='all' is not allowed any more")
    
    moment <- match.arg(moment)
    
    # Check if x is an object of "fpcad" class
    if (!is.fmdsd(x))
      stop("arg1 must be an object of class 'fmdsd'")
    
    if (length(nscore) > 1) {
      warning(paste0("Since dad-4, nscore can only be a single numeric value.\n nscore is set to ", nscore[1]))
      nscore <- nscore[1]
    }
    
    # Read scores
    coor <- x$scores
    group.name <- coor[[1]]
    rownames(coor)=group.name
    matcoor=as.data.frame(coor[1+nscore], stringsAsFactors = TRUE)
    
    colnoms=x$variables
    p = length(colnoms)
    
    matmoments=matrix(nrow=length(group.name), ncol=0)
    colNames=character(0)
    
    switch(moment,
            mean=
              {# Compute and store the means 
                moyL <- x$means
                matmoments=cbind(matmoments, matrix(unlist(moyL),byrow=TRUE,ncol=p))
                colNames=append(colNames, paste("mean",colnoms,sep="."))
              },
            sd=
              {# Standard deviations: storage in columns
                if (p > 1)
                {covL<-lapply(x$variances, diag)
                } else
                {covL<-x$variances
                }
                matmoments=cbind(matmoments, sqrt(matrix(unlist(covL),byrow=TRUE,ncol=p)))
                colNames=append(colNames, paste("sd",colnoms,sep="."))
              },
            var=
              {# Variances Storage in columns
                if (p >1)
                {covL<-lapply(x$variances, diag)
                } else
                {covL<-x$variances
                }
                matmoments=cbind(matmoments, matrix(unlist(covL),byrow=TRUE,ncol=p))
                rownames(matmoments)=group.name
                colNames=append(colNames, paste("var",colnoms,sep="."))
              },
            cov=
              {# Covariances
                # Storage of variances and covariances in columns
                covL<-x$variances
                matcov=matrix(unlist(covL),byrow=TRUE,ncol=p^2)
                if (p > 1)
                { # Select and name the columns containing the covariances
                  nom.col.cov=factor()
                  num.col.cov=factor()
                  for(i in 1:(p-1)){
                    num.col.cov.i=seq((i-1)*p+i+1,i*p,by=1)
                    nom.col.cov.i=paste("cov",paste(colnoms[i],colnoms[(i+1):p],
                                                    sep="."),sep=".")
                    num.col.cov=append(num.col.cov,num.col.cov.i)
                    nom.col.cov=append(nom.col.cov,nom.col.cov.i)
                  }
                  # Variance matrix of the variances and covariances (without 
                  # replication)
                  matmoments=cbind(matmoments, matcov[,num.col.cov])
                  colNames=append(colNames, nom.col.cov)
                } else
                { stop("The covariances cannot be computed\n(The densities are univariate)")
                }
              },
            cor=
              {# Correlations: storage in columns
                if (p > 1)
                { # Add the correlations
                  matcor=matrix(unlist(x$correlations),byrow=TRUE,ncol=p^2)
                  # Select and name the columns containing the correlations
                  num.col.cor=factor()
                  nom.col.cor=factor()
                  for(i in 1:(p-1))
                  {num.col.cor.i=seq((i-1)*p+i+1,i*p,by=1)
                  nom.col.cor.i=paste("cor",paste(colnoms[i],colnoms[(i+1):p],
                                                  sep="."),sep=".")
                  num.col.cor=append(num.col.cor,num.col.cor.i)
                  nom.col.cor=append(nom.col.cor,nom.col.cor.i)
                  }
                  # Matrix of variable correlations (without repetition)
                  matcor=as.matrix(matcor[,num.col.cor])
                  colNames=append(colNames, nom.col.cor)
                  matmoments=cbind(matmoments, matcor)
                } else
                { stop("The correlations cannot be computed\n(The densities are univariate)")
                }
              },
            skewness=
              {# Skewness: storage in columns
                skewnessL <- x$skewness
                matmoments=cbind(matmoments, matrix(unlist(skewnessL),byrow=TRUE,ncol=p))
                colNames=append(colNames, paste("skewness",colnoms,sep="."))
              },
            kurtosis=
              {# Kurtosis: storage in columns
                kurtosisL <- x$kurtosis
                matmoments=cbind(matmoments, matrix(unlist(kurtosisL),byrow=TRUE,ncol=p))
                colNames=append(colNames, paste("kurtosis",colnoms,sep="."))
              }
    )
    matmoments=as.data.frame(matmoments, stringsAsFactors = TRUE)
    rownames(matmoments)=group.name
    colnames(matmoments)=colNames
    
    # Correlations between scores and means, variances, covariances, correlations,
    # skewness and kurtosis
    pearson.mat=cor(matmoments,matcoor)
    spearman.mat=cor(matmoments,matcoor, method="spearman")
    
    # Crossed figures                
    dev.pdf <- (.Device != "pdf")&(ncol(matmoments) > 9)
    if (dev.pdf)
      warning(paste(ncol(matmoments), "graphs displayed. Consider producing PDF graphics."))
    # {namePDF <- paste(as.character(x$call)[1:2], collapse = "_")
    # namePDF <- paste(namePDF, "pdf", sep = ".")
    # warning(paste("Due to the high number of variables, the graphics were displayed in ", namePDF, sep = ""))
    # pdf(namePDF)}
    # 
    plotframes(x=matcoor, y=matmoments, font.size=10, ylab = moment)
    
    # if (dev.pdf)
    # {dev.off(which = dev.list()["pdf"])}
    
    # Display correlations
    cat("Pearson correlations between scores and moments\n")
    print(round(pearson.mat, 2))
    cat("Spearman correlations between scores and moments\n")
    print(round(spearman.mat, 2))
    
    return(invisible(list(pearson=pearson.mat, spearman=spearman.mat)))
  }
