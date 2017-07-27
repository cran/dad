fhclustd <-
function(xf, gaussiand=TRUE, kern = NULL, windowh=NULL,
			normed=TRUE, centered=FALSE, data.centered=FALSE, data.scaled=FALSE,
      common.variance=FALSE, sub.title="", filename=NULL,
      method.hclust = "complete", members = NULL)
{
#require(stats)

#---------------
# Preliminaries
#---------------
if (!is.folder(xf)){
  stop("fpcad applies to an object of class 'folder'.\nNotice that for versions earlier than 2.0, fpcad applied to a data frame.")
}
x <- as.data.frame(xf)

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
if (!prod(apply(as.data.frame(x[,1:p]), 2, is.numeric)))
  stop("The variables must be numeric!")
if (max(is.na(x)) == 1)
  stop("There are NAs in the folder")
# on the window or window parameter
if (!is.null(windowh))
  {if (is.numeric(windowh))
    {if (length(windowh) > 1)
      {stop("windowh must be either a numeric value, either a list of matrix")
      }
    if (windowh < .Machine$double.eps)
      {stop("windowh must be strictly positive!")
      }
    } else 
    {if (is.list(windowh))
      {if (is.null(names(windowh)))
         {stop("The elements of the windowh list must be named")
         } else 
         {if (min(names(windowh)==group.name)<1)
            {stop("The names of the windowh list must be the group names")
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

# For now, the only choice of kernel is the Gaussian kernel.
if (!gaussiand)
  {if (is.null(kern))  kern = "gauss" }  else
  kern = ""

# Control and error message
if (min(table(group)) <= 1)
  stop("There should be more than one observation in each group")

#---------------
# Calculus of the distance matrix
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
#       AMISE window (internally computed by the "bandwidth.parameter" function).
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
  # Case: multivariate, gaussian distributions
  mg.. =
      {xdist <- matdistl2d(x, method = "gaussiand")
       },
  # Case univariate, gaussian distributions 
  ug.. =  
      {xdist <- matdistl2d(x, method = "gaussiand")
      },
  # Case: multivariate, non Gaussian distribution, density estimated using 
  # Gaussian kernel and AMISE window 
  mnga =
      {xdist <- matdistl2d(x, method = "kern")
      },
  # Case univariate, non gaussian distributions estimated by gaussian kernel
  # method, and AMISE windows 
  unga =
      {xdist <- matdistl2d(x, method = "kern")
      },
  # Case: multivariate, non gaussian distributions estimed by gaussian kernel
  # method, and bandwith parameter, common to all densities, given by the user
  mngn =
      {nbL<-by(x[,1:p],INDICES=group,FUN=nrow);
      # Multiplication of the variance by the window parameter
      varLwL<-varL
      for (i in 1:nb.groups)
        {varLwL[[i]]<-varL[[i]]*(windowh^2)}
      xdist <- matdistl2d(x, method = "kern", varwL = varLwL)
      },
  # Case univariate, non gaussian distributions estimed by gaussian kernel
  # method, and bandwith parameter, common to all densities, given by the user    
  ungn =
      {nbL<-by(as.data.frame(x[,1:p]),INDICES=group,FUN=NROW);
      # Multiplication of the variance by the window
      varLwL<-varL
      for (i in 1:nb.groups)
        {varLwL[[i]]<-varL[[i]]*(windowh^2)}
      xdist <- matdistl2d(x, method = "kern", varwL = varLwL)
      },
  # Case: multivariate, non gaussian distributions estimated by gaussian kernel
  # method, and windows given as a list of matrices
  mngl =
      {xdist <- matdistl2d(x, method = "kern", varwL = windowh)
      },
  
    # Case univariate, non gaussian distributions estimated by gaussian kernel
    # method, and windows given as a list of numbers
  ungl =
      {xdist <- matdistl2d(x, method = "kern", varwL = windowh)
      }
  )
# End of the computation of the distance matrix

#Creation of the tree
xclust <- hclust(xdist, method = method.hclust, members = members)

results <- list(distances = xdist, clust = xclust)
class(results) <- "fhclustd"

# Returning the result
return(results)
}
