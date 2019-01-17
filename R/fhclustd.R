fhclustd <-
function(xf, gaussiand=TRUE, distance = "jeffreys", kern = NULL, windowh=NULL,
			normed=(distance == "l2"), data.centered=FALSE, data.scaled=FALSE,
      common.variance=FALSE, sub.title="", filename=NULL,
      method.hclust = "complete",# members = NULL, 
			group.name="group")
{
#require(stats)

#---------------
# Preliminaries
#---------------
if (!is.folder(xf)){
  stop("fhclustd applies to an object of class 'folder'.\nNotice that for versions earlier than 2.0, fhclustd applied to a data frame.")
}
x <- as.data.frame(xf, group.name = group.name)

# p denotes the dimension of the data
p<-ncol(x)-1;

# The initial data is preserved in x0
# (if the data are centered or reduced, x will contain them)
x0<-x

# Rename the last column of x as group
last.column.name=colnames(x)[ncol(x)]
colnames(x)[ncol(x)] <- "group"
group<-as.factor(x$group);
nb.groups<-length(levels(group));
group.name<-levels(group);

# Control and error message
# on data
if (!all(apply(as.data.frame(x[,1:p]), 2, is.numeric)))
  stop("The variables must be numeric!")
if (any(is.na(x)))
  stop("There are NAs in the folder")
# on the window or window parameter
if (!is.null(windowh))
{if (is.numeric(windowh))
{if (length(windowh) > 1)
{stop("windowh must be either a numeric value, or a list of matrix")
}
  if (windowh < .Machine$double.eps)
  {stop("windowh must be strictly positive!")
  }
} else 
{if (is.list(windowh))
{if (is.null(names(windowh)))
{stop("the elements of the windowh list must be named")
} else 
{if (min(names(windowh)==group.name)<1)
{stop("the names of the windowh list must be the group names")
}
}
  if (p >1)
  {if (min(unlist(lapply(windowh, det))) < .Machine$double.eps)
  {stop("All elements of windowh must be positive matrices!")
  }
  } else               
  {if (min(unlist(windowh)) < .Machine$double.eps)
  {stop("All elements of windowh must be strictly positive!")
  }
  }
}
}
}

# Only distances available: "l2" (L^2 distance), "hellinger" (Hellinger/Matusita distance),
# "jeffreys" (symmetric Kullback-Leibler divergence) or "wasserstein" (Wasserstein distance).
if (! distance %in% c("l2", "hellinger", "jeffreys", "wasserstein"))
  stop("'distance' argument must be 'l2', 'hellinger', 'jeffreys' or 'wasserstein'.")

if (distance %in% c("hellinger", "jeffreys", "wasserstein"))
{
  # For the Hellinger distance, the Jeffreys divergence or the Wasserstein distance,
  # the densities are considered Gaussian, and the densities are estimated
  # using the parametric method.
  if (!gaussiand)
  {
    warning(paste0("distance=", distance, " is only avalaible for Gaussian densities.\ngaussiand is set to TRUE"))
    gaussiand <- TRUE
  }
  if (!is.null(windowh) | !is.null(kern))
  {
    warning(paste0("The kernel method is not available when distance=", distance, ".\nThe parametric (Gaussian) method is used for the estimation of the densities."))
  }
  
  if (normed) {
    warning("If distance=", distance, ", the densities cannot be normed.\n normed is set to FALSE.")
    normed <- FALSE
  }
}

# For now, the only choice of kernel is the Gaussian kernel.
if (!gaussiand)
{if (is.null(kern))  kern = "gauss" }  else
  kern = ""

# Control and error message
if (min(table(group)) <= 1)
  stop("There should be more than one observation in each group")

# Mean per group
moyL<-by(as.data.frame(x[,1:p]),INDICES=group,FUN=colMeans);
moyL0<-moyL
if(data.centered)
{# Centering data
  for (i in 1:nb.groups)
  {moyL[[i]]<-numeric(p)
  x[x$group==group.name[i],1:p]=scale(x[x$group==group.name[i],1:p],scale=F)
  }
}

# Variance per group
varL<-by(as.data.frame(x[,1:p]),INDICES=group,FUN=var);
varL0<-varL

# Correlation matrix or correlation coefficient per group
corL=varL
if (p>1)
{corL<-by(as.data.frame(x[,1:p]),INDICES=group,FUN=cor);
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

if(common.variance)
{for (i in 1:nb.groups)
{varL[[i]]<-var(x[,1:p])
}
}

# Skewness et kurtosis coefficients per group
#require(e1071)
skewnessL <- by(as.data.frame(x[, 1:p]), INDICES=group, FUN=apply, 2, skewness)
kurtosisL <- by(as.data.frame(x[, 1:p]), INDICES=group, FUN=apply, 2, kurtosis)

switch(distance,
       "l2" = {
         #---------------
         # Calculus of the inner products matrix W
         #---------------
         choix = character(4)
         # Choice of the dimension
         # "m" : multivariate ; "u" : univariate
         if (p > 1)              
         {choix[1] = "m"
         }  else
         {choix[1] = "u"
         }
         # Choice of the distribution type
         # "g" : gaussian distributions; "n" : non gaussian distributions 
         if (gaussiand)           
         {choix[2] = "g"                    
         }  else
         {choix[2] = "n"
         }
         # Choice of the kernel 
         # "g" : gaussian kernel; "." : not applicable
         # This option offers a limited choice as the only available kernel is the
         # gaussian kernel 
         if (kern == "gauss")
         {choix[3] = "g"
         }  else
         {choix[3] = "."
         }
         # Choice of the window 
         # The window is given by the user in the "windowh" parameter as 
         # "l" : list of (definite positive) matrices
         # "n" : positive number (common to all densities) with which the variance 
         #       matrices are multiplied 
         # "a" : NULL, that is the matrice variance of each density is multiplied by the 
         #       AMISE window (internally computed by the "" function).
         # "." : not applicable
         if (gaussiand)        
         {choix[4] = "."
         }  else
         {if (is.null(windowh))
         {choix[4] = "a"
         }  else
         {if (p == 1)
         {if (length(windowh) == 1)
         {choix[4] = "n"
         } else
         {choix[4] = "l"
         }
         }  else
         {if (is.list(windowh))
         {choix[4] = "l"
         }  else
         {choix[4] = "n"
         }
         } 
         }
         }
         
         choix = paste(choix, collapse = "")
         
         # Calculus of the inner products between densities
         switch(choix,
                # Case: multivariate, gaussian distribution and estimated parameters
                mg.. =
                {W = matipl2dpar(moyL, varL)
                },
                # Case univariate, gaussian distributions with parametres internally estimed 
                ug.. =  
                {W = matipl2dpar(moyL, varL)
                },
                # Case: multivariate, non Gaussian distribution, density estimated using 
                # Gaussian kernel and AMISE window 
                mnga =
                {nbL<-by(x[,1:p],INDICES=group,FUN=nrow);
                wL<-bandwidth.parameter(p,nbL)
                # Multiplication of the variance by the window parameter
                varLwL<-varL
                for (i in 1:nb.groups)
                {varLwL[[i]]<-varL[[i]]*(wL[[i]]^2)}
                W = matipl2d(x, method = "kern", varLwL)
                },
                # Case univariate, non gaussian distributions estimated by gaussian kernel
                # method, and AMISE windows 
                unga =
                {nbL<-by(as.data.frame(x[,1:p]),INDICES=group,FUN=NROW);
                wL<-bandwidth.parameter(p,nbL)
                # Multiplication of the variance by the window parameter
                varLwL<-varL
                for (i in 1:nb.groups)
                {varLwL[[i]]<-varL[[i]]*(wL[[i]]^2)
                }
                W = matipl2d(x, method = "kern", varLwL)
                },
                # Case: multivariate, non gaussian distributions estimed by gaussian kernel
                # method, and bandwith parameter, common to all densities, given by the user
                mngn =
                {nbL<-by(x[,1:p],INDICES=group,FUN=nrow);
                # Multiplication of the variance by the window parameter
                varLwL<-varL
                for (i in 1:nb.groups)
                {varLwL[[i]]<-varL[[i]]*(windowh^2)}
                W = matipl2d(x, method = "kern", varLwL)
                },
                # Case univariate, non gaussian distributions estimed by gaussian kernel
                # method, and bandwith parameter, common to all densities, given by the user    
                ungn =
                {nbL<-by(as.data.frame(x[,1:p]),INDICES=group,FUN=NROW);
                # Multiplication of the variance by the window
                varLwL<-varL
                for (i in 1:nb.groups)
                {varLwL[[i]]<-varL[[i]]*(windowh^2)}
                W = matipl2d(x, method = "kern", varLwL)
                },
                # Case: multivariate, non gaussian distributions estimated by gaussian kernel
                # method, and windows given as a list of matrices
                mngl =
                {W = matipl2d(x, method = "kern", windowh)
                },
                
                # Case univariate, non gaussian distributions estimated by gaussian kernel
                # method, and windows given as a list of numbers
                ungl =
                {W = matipl2d(x, method = "kern", windowh)
                }
         )
         
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
         
         matdist <- diag(0, nb.groups, nb.groups)
         dimnames(matdist) <- list(levels(group), levels(group))
         if (!normed)
         {for (i in 2:nb.groups)  for (j in 1:(i-1))
         {matdist[i, j] <- matdist[j, i] <- sqrt(W[i, i] + W[j, j] - 2*W[i, j])
         }
         }   else
         {for (i in 2:nb.groups)  for (j in 1:(i-1))
         {matdist[i, j] <- matdist[j, i] <- sqrt(2 - 2*W[i, j])
         }
         }
         matdist <- as.dist(matdist)
       },
       "hellinger" = {
         matdist <- mathellinger(xf)
       },
       "jeffreys" = {
         matdist <- matjeffreys(xf)
       },
       "wasserstein" = {
         matdist <- matwasserstein(xf)
       })

#Creation of the tree
xclust <- hclust(matdist, method = method.hclust, members = NULL)

results <- list(distances = matdist, clust = xclust)
class(results) <- "fhclustd"

# Returning the result
return(results)
}
