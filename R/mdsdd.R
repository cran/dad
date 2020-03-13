mdsdd <-
function(xf, distance = c("l1", "l2", "chisqsym", "hellinger", "jeffreys", "jensen", "lp"),
         nb.factors=3, nb.values=10, association = c("cramer", "tschuprow", "pearson", "phi"),
         sub.title="", plot.eigen=TRUE, plot.score=FALSE, nscore=1:3, filename=NULL,
         group.name="group", add = TRUE, p)
{
#require(e1071)

#---------------
# Preliminaries
#---------------
  if (!is.folder(xf) & !all(sapply(xf,is.array))){
    stop("mdsdd applies to an object of class 'folder' or to a list of arrays or tables.")
  }
  
  if (is.folder(xf)) {
    # Convert the data folder into a data frame
    x <- as.data.frame(xf, group.name = group.name)
    
    # Rename the last column of x as 'group'
    colnames(x)[ncol(x)] <- "group"
    group <- as.factor(x$group)
    nb.groups <- length(levels(group))
    groups.name <- levels(group)
    vars.name <- colnames(x)[1:(ncol(x)-1)]
    levels.name <- list()
    for(j in vars.name) {levels.name = c(levels.name, list(levels(x[,j])))}
    # Controls and error messages
    # on data
    if (any(is.na(x)))
      stop("There are NAs in the folder")
    # Computes the joint frequency distribution per group
    tab <- lapply(xf, table)
  } else {
    # Check if all elements of the list xf are arrays with the same dimnames
    nomdim <- lapply(xf, dimnames)
    identdim <- sapply(nomdim, function(x) identical(x, nomdim[[1]]))
    if (!all(identdim))
      stop("If xf is a list of arrays, all its components must have the same dimensions and dimension names.")
    
    tab <- xf
    
    # Check if the elements of tab are arrays with non-negative elements
    is.negative <- sapply(xf, function(x) any(x < 0))
    if (any(is.negative)) {
      stop("All elements of xf must be arrays with non negative elements.")
    }
    
    nb.groups <- length(tab)  # number of arrays (or groups)
    groups.name <- names(tab)  # vector of the names of the arrays 
    # Controls that all the names are different
    vars.name <- names(dimnames(tab[[1]]))  # vector of the names of the discrete variables (the same for all arrays)
    # Controls that are the same
    levels.name = dimnames(tab[[1]])  # list of the level names per dimension (the same for all arrays)
  }
  nb.vars = length(vars.name) # number of discrete variables
  dims <- dim(tab[[1]]) # numbers of levels (one number per discrete variable)

  # Association measure: it can be "cramer" (Cramer's V), "tschuprow" (Tschuprow's T),
  # "pearson" (Pearson's contingency coefficient) or "phi".
  association <- match.arg(association)
  assocfc <- switch(association,
                    cramer = CramerV,
                    tschuprow = TschuprowT,
                    pearson = ContCoef,
                    phi = Phi
  )
  
  # Computes the joint probability distribution per group 
  freq <- lapply(tab, function(x){x/sum(x)})
  
  # Computes the marginal distributions per group
  for (idx in 1:nb.vars) {
    isum <- function(x) {
      # function to compute the sum of the elements of the array x over dimension idx
      apply(x, MARGIN = idx, FUN = sum)
    }
    
    # Marginal distributions of the idx-th variable:
    # for each element of freq, sum of its elements over dimension idx
    listpmargin <- unlist(lapply(freq, isum))
    # Store it in a matrix (columns = variables, rows = groups)
    matpmargin <- matrix(listpmargin, byrow = TRUE, ncol = dims[idx])
    colnames(matpmargin) <- paste(vars.name[idx], levels.name[[idx]], sep = ".")
    # Add matpmargin to the matrix of all probabilities
    if(idx > 1) {
      matprob <- cbind(matprob, matpmargin)
    } else {
      matprob = matpmargin
      rownames(matprob) = groups.name
    }
  }
  matprob = as.data.frame(matprob, stringsAsFactors = TRUE)
  
  if (nb.vars > 1) {
    
    #############################
    # For each group (occasion), calculate the probabilities of occurence
    #     of each couple of levels of each couple of factors.
    #############################
    
    # For group 1
    nog = 1
    xg = freq[[nog]]  # Contingency table
    nameg = groups.name[nog]  # Name of the group

    vvprob = numeric()  # Name of the vector of the computed probabilities
    dfcolsname = character()  # Name of the columns of the data frame
    colsnamev1v2 = character()
    for(novar1 in 1:(nb.vars-1)) {
      vprobvar1 = numeric()
      lvar1 = paste0("V", novar1, ".", levels.name[[novar1]])
      vprob = diag(apply(xg, novar1, sum))
      for(novar2 in ((novar1+1):nb.vars)) {
        # Probabilities of occurence of each couple of modalities of these two variables
        vprob = apply(xg, c(novar1,novar2), sum)
        vecvprob = as.vector(vprob)
        vprobvar1 = c(vprobvar1, vecvprob)
        lvar2 = paste0("V", novar2, ".", levels.name[[novar2]])
        colsnamev1v2 = c(colsnamev1v2,
                         apply(expand.grid(lvar1,lvar2), 1, function(x) paste(x, collapse=":")))
      }
      vvprob = c(vvprob, vprobvar1)
    }
    dfcolsname = c(dfcolsname, colsnamev1v2)

    # Initialisation of the data frame dfjp. Its rows are the groups (occasions)
    # and its columns are the probabilities of each couple of modalities,
    # First row: the probabilities for the 1st group.
    dfjp = data.frame(t(vvprob), row.names=nameg, stringsAsFactors = TRUE)
    names(dfjp) = dfcolsname
    
    # For the other groups
    for(nog in 2:nb.groups) {
      xg = freq[[nog]]
      nameg = groups.name[nog]

      vvprob = numeric()
      for(novar1 in 1:(nb.vars-1)) {
        vprobvar1 = numeric()
        vprob = diag(apply(xg, novar1, sum))
          for(novar2 in ((novar1+1):nb.vars)) {
            # Probabilities of occurence of each couple of modalities of these two variables
            vprob = apply(xg, c(novar1,novar2), sum)
            vecvprob = as.vector(vprob)
            vprobvar1 = c(vprobvar1, vecvprob)
          }
          vvprob = c(vvprob, vprobvar1)
      }
      dfjpg = data.frame(t(vvprob), row.names=nameg, stringsAsFactors = TRUE)
      names(dfjpg) = dfcolsname
      dfjp = rbind(dfjp, dfjpg)
    }
    
    #############################
    # Computation of the pairwise-associations between the variables
    #############################
    
    # Computation of the pairwise-associations between the variables in group 1
    assocL <- list()
    
    nog = 1
    xg = freq[[nog]]  # Contingency table of all variables in the 1st group
    nameg = groups.name[nog]  # Name of the group
    matassoc <- matrix(nrow = nb.vars, ncol = nb.vars, dimnames = list(vars.name, vars.name))
    
    for(novar1 in 1:(nb.vars-1)) {
      tab1 <- apply(xg, novar1, sum)
      nlevnonnul1 <- sum(tab1 > 0)
      whichnul1 <- which(tab1 == 0)
      if (nlevnonnul1 == 1) {
        matassoc[novar1, (novar1+1):nb.vars] = matassoc[(novar1+1):nb.vars, novar1] = 0
      } else {
        for(novar2 in ((novar1+1):nb.vars)) {
          tab2 <- apply(xg, novar2, sum)
          nlevnonnul2 <- sum(tab2 > 0)
          whichnul2 <- which(tab2 == 0)
          if (nlevnonnul2 == 1) {
            matassoc[novar1, novar2] = matassoc[novar2, novar1] = 0
          } else {
            vprob = apply(xg, c(novar1,novar2), sum)
            # Suppression des lignes vides
            if (length(whichnul1) > 0)
              vprob = vprob[-whichnul1, ]
            # Suppression des colonnes vides
            if (length(whichnul2) > 0)
              vprob = vprob[, -whichnul2]
            # Association measure between these two variables (after deleting the rows/columns entirely 0)
            matassoc[novar1, novar2] = matassoc[novar2, novar1] = assocfc(vprob)
          }
        }
      }
    }
    assocL[[nameg]] <- matassoc
    
    # Computation of the pairwise-associations between the variables in the other groups

    for(nog in 2:nb.groups) {
      matassoc <- matrix(nrow = nb.vars, ncol = nb.vars, dimnames = list(vars.name, vars.name))
      xg = freq[[nog]]
      nameg = groups.name[nog]

      for(novar1 in 1:(nb.vars-1)) {
        tab1 <- apply(xg, novar1, sum)
        nlevnonnul1 <- sum(tab1 > 0)
        whichnul1 <- which(tab1 == 0)
        if (nlevnonnul1 == 1) {
          matassoc[novar1, (novar1+1):nb.vars] = matassoc[(novar1+1):nb.vars, novar1] = 0
        } else {
          for(novar2 in ((novar1+1):nb.vars)) {
            tab2 <- apply(xg, novar2, sum)
            nlevnonnul2 <- sum(tab2 > 0)
            whichnul2 <- which(tab2 == 0)
            if (nlevnonnul2 == 1) {
              matassoc[novar1, novar2] = matassoc[novar2, novar1] = 0
            } else {
              vprob = apply(xg, c(novar1,novar2), sum)
              # Suppression des lignes vides
              if (length(whichnul1) > 0)
                vprob = vprob[-whichnul1, ]
              # Suppression des colonnes vides
              if (length(whichnul2) > 0)
                vprob = vprob[, -whichnul2]
              # Association measure between these two variables (after deleting the rows/columns entirely 0)
              matassoc[novar1, novar2] = matassoc[novar2, novar1] = assocfc(vprob)
            }
          }
        }
      }
      assocL[[nameg]] <- matassoc
    }
    
    
    
    
    
    
    
  } else {
    dfjp <- NULL
    assocL <- NULL
  }
  
  # Only distances available: "lp" (L^p distance), "hellinger" (Hellinger/Matusita distance),
  # "chisqsym" (symmetric Chi-squared distance), "jeffreys" (symmetric Kullback-Leibler
  # divergence), "jensen" (Jensen-Shannon distance)
  distance <- match.arg(distance)
  
  distance.printing <- distance
  if (distance == "l1") {
    distance <- "lp"
    p <- 1
    distance.printing <- "l1"
  }
  if (distance == "l2") {
    distance <- "lp"
    p <- 2
    distance.printing <- "l2"
  }
  if (distance == "lp") {
    if (missing(p))
      p <- 1
    distance.printing <- paste("lp with p =", p)
  }
  
  # Control and error message
  # Control of nb.factors and nb.values
  if (nb.groups < nb.values)
   {nb.values <- nb.groups - 1
   }
  if (nb.groups <= nb.factors)
   {nb.factors <- nb.groups - 1
   }
  
  switch(distance,
         "lp" = {
           matdist <- matddlppar(freq, p = p)
         },
         "hellinger" = {
           matdist <- matddhellingerpar(freq)
         },
         "chisqsym" = {
           matdist <- matddchisqsympar(freq)
         },
         "jeffreys" = {
           matdist <- matddjeffreyspar(freq)
         },
         "jensen" = {
           matdist <- matddjensenpar(freq)
         }
        )
  
  # Controls that all the distances inter-groups are finite
  if( all(is.finite(matdist)) ) {
    cmds <- cmdscale(matdist, k = nb.factors, eig = TRUE, add = add, x.ret = TRUE)
    
    # Eigenvalues to display
    ep <- cmds$eig
    epaff <- ep[1:nb.values]
    
    # Scores to display
    coor <- cmds$points
  } else {
    warning("Some distances between groups are infinite. The choice of distance is not pertinent")
    results <- list( call = match.call(),
                     group = group.name,
                     variables = vars.name,
                     d = distance.printing,
                     inertia = NULL,
                     scores = NULL)
    # Estimated joint distributions
    results$jointp <- freq
    # Estimated marginal distributions
    results$margins <- list(margin1 = matprob, margin2 = dfjp)
    # Association measures between distributions
    results$associations <- assocL
    if (!is.null(assocL)) {
      attr(results$associations, "measure") <- association
    } else {
      attr(results$associations, "measure") <- NULL
    }
    class(results) <- "mdsdd"
    
    return(results)
  }
  
  # Creation of the list of results in an object of class 'fmdsd' :
  results <- list( call = match.call(),
                   group = group.name,
                   variables = vars.name,
                   d = distance.printing,
                   inertia = data.frame(eigenvalue=epaff,
                                        inertia=round(1000*epaff/sum(abs(ep)))/10,
                                        stringsAsFactors = TRUE),
                   scores = data.frame(groups.name,PC=coor, stringsAsFactors = TRUE))
  
  # Change of the name of the grouping variable in the data frames in results
  colnames(results$scores)[1] = group.name
  # Estimated joint distributions
  results <- c(results, list(jointp = freq))
  # Estimated marginal distributions
  results$margins <- list(margin1 = matprob, margin2 = dfjp)
  # Association measures between distributions
  results$associations <- assocL
  if (!is.null(assocL)) {
    attr(results$associations, "measure") <- association
  } else {
    attr(results$associations, "measure") <- NULL
  }
  
  class(results) <- "mdsdd"
  
  # Save this list in a file (if a filename is given)
  if (! is.null(filename))
    {
    save(results,file = filename)
  }
  
  # Barplot of the inertia
  if (plot.eigen)
    {
    inertia=results$inertia$inertia
    barplot(inertia, main="Inertia",
            names.arg=1:length(inertia),
            cex.names=1)
  }
  
  # Plotting the scores on the principal planes (1,2),(1,3) et (2,3)
  # Plane (a,b)
  if (plot.score)
    {
    plot(results, nscore=nscore, sub.title=sub.title)
  }
  
  # Returning the list of results
  return(results)
}
