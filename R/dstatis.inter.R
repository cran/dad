dstatis.inter <- function(xf, normed = TRUE, centered = TRUE, data.scaled=FALSE, 
      nb.factors=3, nb.values=10, sub.title="",
			plot.eigen=TRUE, plot.score=FALSE, nscore=1:3, group.name="group", filename=NULL)
{
  #---------------
  # Preliminaries
  #---------------
  if (!is.folder(xf)){
    stop("dstatis.inter applies to an object of class 'folder'.")
  }
  x <- as.data.frame(xf, group.name = group.name, stringsAsFactors = TRUE)
  
  # p denotes the dimension of the data
  p<-ncol(x)-1;
  
  # Rename the last column of x as "group"
  last.column.name=colnames(x)[ncol(x)]
  colnames(x)[ncol(x)] <- "group"
  group<-as.factor(x$group);
  nb.groups<-length(levels(group));
  group.name<-levels(group);
  
  # Control and error message
  # on data
  if (!all(apply(as.data.frame(x[,1:p], stringsAsFactors = TRUE), 2, is.numeric)))
    stop("The variables must be numeric!")
  if (any(is.na(x)))
    stop("There are NAs in the folder")
  
  # Control and error message
  if (min(table(group)) <= 1)
    stop("There should be more than one observation in each group")
  
  # Control of nb.factors and nb.values
  if (nb.groups < nb.values)
   {nb.values <- nb.groups ;
   }
  if (nb.groups < nb.factors)
   {nb.factors <- nb.groups
   }
  
  # Mean per group
  moyL<-by(as.data.frame(x[,1:p], stringsAsFactors = TRUE),INDICES=group,FUN=colMeans);
  moyL0<-moyL
  
  # Variance per group
  varL<-by(as.data.frame(x[,1:p], stringsAsFactors = TRUE),INDICES=group,FUN=var);
  varL0<-varL
  
  # Correlation matrix or correlation coefficient per group
  corL=varL
  if (p>1)
     {corL<-by(as.data.frame(x[,1:p], stringsAsFactors = TRUE),INDICES=group,FUN=cor);
     } else
     {for (i in 1:nb.groups)
       {corL[[i]]<-1
       }
     }
  
  if(data.scaled)
   {varL<-corL
     for (i in 1:nb.groups)
       {x[x$group==group.name[i],1:p]=scale(x[x$group==group.name[i],1:p])
       }
   }
  
  # Skewness et kurtosis coefficients per group
  #require(e1071)
  skewnessL <- by(as.data.frame(x[, 1:p], stringsAsFactors = TRUE), INDICES=group, FUN=apply, 2, skewness)
  kurtosisL <- by(as.data.frame(x[, 1:p], stringsAsFactors = TRUE), INDICES=group, FUN=apply, 2, kurtosis)
  
  #---------------
  # Calculus of the matrix W to diagonalize
  #---------------
  # choix = character(1)
  # Choice of the dimension
  # "m" : multivariate ; "u" : univariate
  if (p > 1)              
    {choix = "m"
    }  else
    {choix = "u"
    }
  
  # Calculus of the inner products between densities
  W<-matrix(0,ncol=nb.groups,nrow=nb.groups);
  switch(choix,
  
    # Case: multivariate
    m =
        {for (i in 1:nb.groups)
           {for (j in 1:i)
              {W[i,j]<-sum(diag(varL[[i]] %*% varL[[j]]))
              }
           }
         },
    # Case: univariate
    u =  
        {for (i in 1:nb.groups)
          {for (j in 1:i)
            {W[i,j]<-varL[[i]] * varL[[j]]
            }
          }
        }
    )
  #
  for (i in 1:(nb.groups-1))
    {for (j in (i+1):nb.groups)                             
      {W[i,j]<-W[j,i] }}
  # End of the computation of the matrix W
  
  norme<-vector("numeric",nb.groups);
  for (i in 1:nb.groups)
   {norme[i]<-sqrt(W[i,i])};
  if(normed)
   {# Calculus of the matrix W of the normed pca
    for (i in 1:nb.groups)
     {for (j in 1:i)
       {W[i,j]<-W[i,j]/(norme[i]*norme[j])}};
    for (i in 1:(nb.groups-1))
     {for (j in (i+1):nb.groups)
       {W[i,j]<-W[j,i]}};
   }
  if(centered)
   {# Calculus of the matrix W of the centred pca
    moyW<-mean(W);
    moycolW<-colMeans(as.data.frame(W, stringsAsFactors = TRUE));
    for (i in 1:nb.groups)
     {for (j in 1:i)
       {W[i,j]<-W[i,j]-moycolW[[i]]-moycolW[[j]]+moyW}};
    for (i in 1:(nb.groups-1))
     {for (j in (i+1):nb.groups)
       {W[i,j]<-W[j,i]}};
   }
   
  #---------------
  # Calculus of the eigenvalues / eigenvectors of the matrix W
  #	Calculus of the scores / contributions / qualities
  #---------------
  ep<-svd(W)
  
  # Eigenvalues to display
  epaff<-ep$d[1:nb.values];
  
  # Scores to display
  valpsqrt<-diag(sqrt(abs(ep$d[1:nb.factors])));
  coor <- ep$u[,1:nb.factors] %*% diag(sign(ep$u[1,1:nb.factors])) %*% valpsqrt ;
  
  # Corresponding qualities
  qual<-coor;
  for (i in 1:nb.groups)
   {qual[i,]<-round(1000*(coor[i,]^2)/W[i,i]) /10};
  
  # Corresponding contributions
  cont<-coor;
  for (j in 1:nb.factors)
   {cont<-round(1000*(ep$u[,1:nb.factors]^2)) /10};
  
  # Change of the names of the grouping variable in the returned results
  names(attributes(moyL)$dimnames) <- last.column.name  
  names(attributes(varL)$dimnames) <- last.column.name
  names(attributes(corL)$dimnames) <- last.column.name
  names(attributes(skewnessL)$dimnames) <- last.column.name
  names(attributes(kurtosisL)$dimnames) <- last.column.name
  
  # Creation of the list of results in an object of class 'fpcad' :
  results <- list( call=match.call(),
    group=last.column.name,
    variables=colnames(x)[1:p],
    inertia=data.frame(eigenvalue=epaff,
            inertia=round(1000*epaff/sum(abs(ep$d)))/10,
            stringsAsFactors = TRUE),
    contributions=data.frame(group.name,PC=cont, stringsAsFactors = TRUE),
    qualities=data.frame(group.name,PC=qual, stringsAsFactors = TRUE),
    scores=data.frame(group.name,PC=coor, stringsAsFactors = TRUE),
    norm = data.frame(group.name,norm=norme, stringsAsFactors = TRUE),
    means=moyL,
    variances=varL,
    correlations=corL,
    skewness=skewnessL,
    kurtosis=kurtosisL);
  # Change of the name of the grouping variable in the data frames in results
  colnames(results$contributions)[1]=last.column.name
  colnames(results$qualities)[1]=last.column.name
  colnames(results$scores)[1]=last.column.name
  colnames(results$norm)[1]=last.column.name
  class(results) <- "dstatis"
  
  # Save this list in a file (if a filename is given)
  if (! is.null(filename))
    {save(results,file = filename)
    }
   
  # Barplot of the inertia
  if (plot.eigen)
    {inertia=results$inertia$inertia
    barplot(inertia, main="Inertia",
            names.arg=1:length(inertia),
            cex.names=1)
    }
   
  # Plotting the scores on the principal planes (1,2),(1,3) et (2,3)
  # Plane (a,b)
  if (plot.score)
    {plot(results, nscore=nscore, sub.title=sub.title)
    }
  
  # Returning the list of results
  return(results)
}
