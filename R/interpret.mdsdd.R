interpret.mdsdd <-
function(x, nscore=1, mma = c("marg1", "marg2", "assoc"), ...) {
  mma <- match.arg(mma)
  
  if (length(nscore) > 1)
    nscore <- nscore[1]
  
  # Read scores
  coor <- x$scores
  group.name <- coor[[1]]
  rownames(coor)=group.name
  matcoor=as.data.frame(coor[1+nscore], stringsAsFactors = TRUE)
  
  colnoms=x$variables
  q = length(colnoms)
  
  # Marginal probabilities for each modality of each variable (jointp = 1) or couple
  # of variables (jointp = 2), or association measures for each couple of variables
  switch(mma,
         marg1 = {
           matprob = x$margins$margin1
         },
         marg2 = {
           matprob = x$margins$margin2
         },
         assoc = {
           # Storage of association measures in columns
           matprob=matrix(nrow=length(group.name), ncol=0)
           colNames=character(0)
           assocL<-x$associations
           matpr=matrix(unlist(assocL),byrow=TRUE,ncol=q^2)
           if (q > 1) {
             # Select and name the columns containing the association measures
             nom.col.assoc=factor()
             num.col.assoc=factor()
             for(i in 1:(q-1)) {
               num.col.assoc.i=seq((i-1)*q+i+1,i*q,by=1)
               nom.col.assoc.i=paste("assoc",paste(colnoms[i],colnoms[(i+1):q],
                                                   sep="."),sep=".")
               num.col.assoc=append(num.col.assoc,num.col.assoc.i)
               nom.col.assoc=append(nom.col.assoc,nom.col.assoc.i)
             }
             # Association matrix of the association measures (without replication)
             matprob=cbind(matprob, matpr[,num.col.assoc])
             # colNames=append(colNames, nom.col.assoc)
             rownames(matprob)=group.name
             colnames(matprob)=nom.col.assoc
           } else {
             stop("The association measures cannot be computed\n(The densities are univariate)")
           }
         }
  )
  
  mmanoms <- colnames(matprob)
  
  # Correlations between scores and marginal probabilities/association measures
  pearson.mat=cor(matprob,matcoor)
  spearman.mat=cor(matprob,matcoor, method="spearman")
  
  # Crossed figures
  dev.pdf <- (.Device != "pdf")&(ncol(matprob) > 36)
  if (dev.pdf)
    warning(paste(ncol(matprob), "graphs displayed. Consider producing PDF graphics."))
  if (dev.pdf) {
    namePDF <- paste(as.character(x$call)[1:2], collapse = "_")
    namePDF <- paste(namePDF, "pdf", sep = ".")
    warning(paste("Due to the high number of variables, the graphics were displayed in ", namePDF, sep = ""))
    pdf(namePDF)
  }
  
  if (mma == "marg1") {
    mmanoms <- unique(sapply(strsplit(mmanoms, ".", fixed = TRUE), "[", 1))
    for (nPC in 1:length(nscore)) {
        for (nom in mmanoms) {
         cnom <- (substring(colnames(matprob), 1, nchar(nom)) == nom)
          matprobnom <- matprob[, cnom]
           plotframes(x=matcoor[,nPC, drop=FALSE], y=matprobnom, font.size=10)
        }
    }
  } else if (mma == "marg2") {
    mmanoms <- data.frame(strsplit(colnames(matprob), ":"), stringsAsFactors = FALSE)
    for (nPC in 1:length(nscore)) {
    for (i1 in 1:(nrow(mmanoms) - 1)) {
      mmanom1 <- mmanoms[i1, ]
      for (i2 in (i1+1):nrow(mmanoms)) {
        mmanom2 <- mmanoms[i2, ]
        matprobnom <- matprob[, paste(mmanom1, mmanom2, sep = ":")]
        plotframes(x=matcoor[, nPC, drop=FALSE], y=matprobnom, font.size=10)
      }
    }
    }
  } else if (mma == "assoc") {
    plotframes(x=matcoor, y=matprob, font.size=10, ylab = mma)
  }

  margassoc <- paste0("probability distributions of each variable"[mma == "marg1"],
                      "probability distributions of each pair of variables"[mma == "marg2"],
                      "association measures"[mma == "assoc"])
  asso.measure <- paste0("(", attr(x$associations, "measure"), ")")
  # Display correlations
  cat(paste0("Pearson correlations between scores and ", margassoc, asso.measure[mma == "assoc"], "\n"))
  print(round(pearson.mat, 2))
  cat(paste0("Spearman correlations between scores and ", margassoc, asso.measure[mma == "assoc"], "\n"))
  print(round(spearman.mat, 2))
  
  return(invisible(list(pearson=pearson.mat, spearman=spearman.mat)))
}
