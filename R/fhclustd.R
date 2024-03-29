fhclustd <-
  function(xf, group.name="group", gaussiand=TRUE,
           distance = c("jeffreys", "hellinger", "wasserstein", "l2", "l2norm"),
           windowh=NULL, data.centered=FALSE, data.scaled=FALSE, common.variance=FALSE,
           sub.title="", filename=NULL, method.hclust = "complete"#, members = NULL
           )
  {
    #---------------
    # Preliminaries
    #---------------
    if (!(is.folder(xf) | is.data.frame(xf))){
      stop("fhclustd applies to a data frame or an object of class 'folder'.")
    }
    
    if (is.folder(xf)) {
      # Convert the data folder into a data frame
      x <- as.data.frame(xf, group.name = group.name)
    }
    if (is.data.frame(xf)) {
      # Is group.name the name of a column of x?
      if (!group.name %in% names(xf))
        stop(paste0("xf has no column named '", group.name, "'."))
      # x will contain the data.frame
      x <- xf[-which(colnames(xf) == group.name)]
      # Last column: the grouping variable
      x[group.name] <- xf[, group.name]
      # Convert the data.frame into a folder
      xf <- as.folder(xf, groups = group.name)
    }
    # p denotes the dimension of the data
    p<-ncol(x)-1;
    
    #### # The initial data is preserved in x0
    #### # (if the data are centered or reduced, x will contain them)
    #### x0<-x
    
    # Rename the last column of x as group
    last.column.name=colnames(x)[ncol(x)]
    colnames(x)[ncol(x)] <- "group"
    group<-as.factor(x$group);
    nb.groups<-length(levels(group));
    levgroup<-levels(group);
    
    # Control and error message
    # on data
    if (!all(apply(as.data.frame(x[,1:p], stringsAsFactors = TRUE), 2, is.numeric)))
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
    {if (min(names(windowh)==levgroup)<1)
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
    
    # Only distances available: "l2" (L^2 distance), "l2norm" (L^2 distance between normalized densities),
    # "hellinger" (Hellinger/Matusita distance), "jeffreys" (symmetric Kullback-Leibler divergence)
    # or "wasserstein" (Wasserstein distance).
    distance <- match.arg(distance)
    
    if ( (!gaussiand) & (distance %in% c("hellinger", "jeffreys", "wasserstein")) ) {
      warning(paste0("distance=", distance, " is not avalaible for non-Gaussian densities.\nSo the argument distance is set to 'l2'."))
      distance <- "l2"
    }
    
    if (gaussiand & !is.null(windowh)) {
      warning("The argument windowh is not taken into account for Gaussian densities.")
    }
    
    # For now, the only choice of kernel is the Gaussian kernel.
    if (!gaussiand)
    {kern = "gauss"}  else
      kern = ""
    
    # Control and error message
    if (min(table(group)) <= 1)
      stop("There should be more than one observation in each group")
    
    # Mean per group
    moyL<-by(as.data.frame(x[,1:p], stringsAsFactors = TRUE),INDICES=group,FUN=colMeans);
    moyL0<-moyL
    if(data.centered)
    {# Centering data
      for (i in 1:nb.groups)
      {moyL[[i]]<-numeric(p)
      x[x$group==levgroup[i],1:p]=scale(x[x$group==levgroup[i],1:p],scale=F)
      }
    }
    
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
    {x[x$group==levgroup[i],1:p]=scale(x[x$group==levgroup[i],1:p])
    }
    }
    
    if(common.variance)
    {for (i in 1:nb.groups)
    {varL[[i]]<-var(x[,1:p])
    }
    }
    
    # Skewness et kurtosis coefficients per group
    #require(e1071)
    skewnessL <- by(as.data.frame(x[, 1:p], stringsAsFactors = TRUE), INDICES=group, FUN=apply, 2, skewness)
    kurtosisL <- by(as.data.frame(x[, 1:p], stringsAsFactors = TRUE), INDICES=group, FUN=apply, 2, kurtosis)
    
    # If distance == "l2norm" (i.e. L^2-distance between normed densities) then:
    #   - dist = "l2"
    #   - normed = TRUE
    if (distance == "l2norm") {
      distance <- "l2"
      normed <- TRUE
    } else {
      normed <- FALSE
    }
    
    if (gaussiand) { # Gaussian case
      
      switch(distance,
             "l2" = {
               #---------------
               # Calculus of the inner products matrix W
               #---------------
               W = matipl2dpar(moyL, varL)
               
               norme<-vector("numeric",nb.groups);
               for (i in 1:nb.groups) {
                 norme[i]<-sqrt(W[i,i])}
               if(normed) {
                 # Calculus of the matrix W of the normed pca
                 for (i in 1:nb.groups) {
                   for (j in 1:i) {
                     W[i,j]<-W[i,j]/(norme[i]*norme[j])
                   }
                 }
                 for (i in 1:(nb.groups-1)) {
                   for (j in (i+1):nb.groups) {
                     W[i,j]<-W[j,i]
                   }
                 }
               }
               
               matdist <- diag(0, nb.groups, nb.groups)
               dimnames(matdist) <- list(levels(group), levels(group))
               if (!normed) {
                 for (i in 2:nb.groups)  for (j in 1:(i-1)) {
                   matdist[i, j] <- matdist[j, i] <- sqrt(W[i, i] + W[j, j] - 2*W[i, j])
                 }
               } else {
                 for (i in 2:nb.groups)  for (j in 1:(i-1)) {
                   matdist[i, j] <- matdist[j, i] <- sqrt(2 - 2*W[i, j])
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
             }
      )
    } else { # Non Gaussian case: Gaussian kernel method
      
      # kern <- "gauss"
      
      # If distance == "l2norm" (i.e. L^2-distance between normed densities) then:
      #   - dist = "l2"
      #   - normed = TRUE
      if (distance == "l2norm") {
        distance <- "l2"
        normed <- TRUE
      } else {
        normed <- FALSE
      }
      
      #---------------
      # Calculus of the inner products matrix W
      #---------------
      choix = character(3)
      # Choice of the dimension
      # "m" : multivariate ; "u" : univariate
      if (p > 1) {
        choix[1] = "m"
      } else {
        choix[1] = "u"
      }
      # Choice of the kernel 
      # "g" : gaussian kernel; "." : not applicable
      # This option offers a limited choice as the only available kernel is the
      # gaussian kernel 
      if (kern == "gauss") {
        choix[2] = "g"
      } else {
        choix[2] = "."
      }
      # Choice of the window 
      # The window is given by the user in the "windowh" parameter as 
      # "l" : list of (definite positive) matrices
      # "n" : positive number (common to all densities) with which the variance 
      #       matrices are multiplied 
      # "a" : NULL, that is the matrice variance of each density is multiplied by the 
      #       AMISE window (internally computed by the "" function).
      # "." : not applicable
      if (is.null(windowh)) {
        choix[3] = "a"
      } else {
        if (p == 1) {
          if (length(windowh) == 1) {
            choix[3] = "n"
          } else {
            choix[3] = "l"
          }
        } else {
          if (is.list(windowh)) {
            choix[3] = "l"
          } else {
            choix[3] = "n"
          }
        }
      }
      choix = paste(choix, collapse = "")
      
      # Calculus of the inner products between densities
      switch(choix,
             # Case: multivariate, non Gaussian distribution, density estimated using 
             # Gaussian kernel and AMISE window 
             mga =
               {nbL<-by(x[,1:p],INDICES=group,FUN=nrow);
               wL<-bandwidth.parameter(p,nbL)
               # Multiplication of the variance by the window parameter
               varLwL<-varL
               for (i in 1:nb.groups)
               {varLwL[[i]]<-varL[[i]]*(wL[[i]]^2)}
               W = matipl2d(xf, method = "kern", varLwL)
               },
             # Case univariate, non gaussian distributions estimated by gaussian kernel
             # method, and AMISE windows 
             uga =
               {nbL<-by(as.data.frame(x[,1:p], stringsAsFactors = TRUE),INDICES=group,FUN=NROW);
               wL<-bandwidth.parameter(p,nbL)
               # Multiplication of the variance by the window parameter
               varLwL<-varL
               for (i in 1:nb.groups)
               {varLwL[[i]]<-varL[[i]]*(wL[[i]]^2)
               }
               W = matipl2d(xf, method = "kern", varLwL)
               },
             # Case: multivariate, non gaussian distributions estimed by gaussian kernel
             # method, and bandwith parameter, common to all densities, given by the user
             mgn =
               {nbL<-by(x[,1:p],INDICES=group,FUN=nrow);
               # Multiplication of the variance by the window parameter
               varLwL<-varL
               for (i in 1:nb.groups)
               {varLwL[[i]]<-varL[[i]]*(windowh^2)}
               W = matipl2d(xf, method = "kern", varLwL)
               },
             # Case univariate, non gaussian distributions estimed by gaussian kernel
             # method, and bandwith parameter, common to all densities, given by the user    
             ugn =
               {nbL<-by(as.data.frame(x[,1:p], stringsAsFactors = TRUE),INDICES=group,FUN=NROW);
               # Multiplication of the variance by the window
               varLwL<-varL
               for (i in 1:nb.groups)
               {varLwL[[i]]<-varL[[i]]*(windowh^2)}
               W = matipl2d(xf, method = "kern", varLwL)
               },
             # Case: multivariate, non gaussian distributions estimated by gaussian kernel
             # method, and windows given as a list of matrices
             mgl =
               {W = matipl2d(xf, method = "kern", windowh)
               },
             
             # Case univariate, non gaussian distributions estimated by gaussian kernel
             # method, and windows given as a list of numbers
             ugl =
               {W = matipl2d(xf, method = "kern", windowh)
               }
      )
      
      norme<-vector("numeric",nb.groups)
      for (i in 1:nb.groups) {
        norme[i]<-sqrt(W[i,i])
      }
      if(normed) {
        # Calculus of the matrix W of the normed pca
        for (i in 1:nb.groups) {
          for (j in 1:i) {
            W[i,j]<-W[i,j]/(norme[i]*norme[j])
          }
        }
        for (i in 1:(nb.groups-1)) {
          for (j in (i+1):nb.groups) {
            W[i,j]<-W[j,i]
          }
        }
      }
      
      matdist <- diag(0, nb.groups, nb.groups)
      dimnames(matdist) <- list(levels(group), levels(group))
      if (!normed) {
        for (i in 2:nb.groups)  for (j in 1:(i-1)) {
          matdist[i, j] <- matdist[j, i] <- sqrt(W[i, i] + W[j, j] - 2*W[i, j])
        }
      } else {
        for (i in 2:nb.groups)  for (j in 1:(i-1)) {
          matdist[i, j] <- matdist[j, i] <- sqrt(2 - 2*W[i, j])
        }
      }
      matdist <- as.dist(matdist)
    }
    
    #Creation of the tree
    xclust <- hclust(matdist, method = method.hclust, members = NULL)
    
    results <- list(distances = matdist, clust = xclust)
    class(results) <- "fhclustd"
    
    # Returning the result
    return(results)
  }
