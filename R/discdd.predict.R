discdd.predict <-
  function(xf, class.var,
           distance =  c("l1", "l2", "chisqsym", "hellinger", "jeffreys", "jensen", "lp"),
           crit = 1, misclass.ratio = FALSE, p)
  {
    # xf:     object of class 'folderh' with 2 data frames.
    # class.var: character. Name of the column of xf[[1]] containing the class variable.
    
    if (!is.list(xf))
      stop("discdd.misclass applies to a folderh with only two data frames or a list of arrays.")
    
    if (!is.folderh(xf)) {
      if (!all(sapply(xf, is.array)))
        stop("discdd.misclass applies to a folderh with only two data frames or a list of arrays.")
      
      if (!is.data.frame(class.var) | ncol(class.var) != 2) {
        stop("If xf is a list of arrays, class.var must be a data.frame with 2 columns named 'group' and 'class'.")
      }
    }
    
    if (!(crit %in% 1:2))
      stop("crit must be 1 or 2.")
    
    # Only distances available: "l1", "l2", "chisqsym", "hellinger", "jeffreys", "jensen", "lp"
    distance <- match.arg(distance)
    
    if (is.folderh(xf)) {
      
      if (length(xf) != 2)
        stop("discdd.misclass applies to a folderh of with only two data frames or a list of arrays.")
      if (ncol(xf[[1]]) < 2)
        stop(paste0("The 1st data frame of xf must have at least two columns (the grouping variable and the classes)."))
      
      x <- xf[[2]]
      j.group <- which(colnames(x) == attr(xf, "keys"))
      if (j.group != ncol(x))
        x <- data.frame(x[-j.group], x[j.group], stringsAsFactors = TRUE)
      classe <- xf[[1]][c(attr(xf, "keys"), class.var)]
      
      # Number of variables:
      q <- ncol(x)-1
      
      # If necessary: change the variables of x into factors:
      for (j in 1:(q+1))
        if (!is.factor(x[, j]))  x[, j] <- as.factor(x[, j])
      
      # The groups
      group<-as.factor(x[, q+1]);
      nb.groups <- nlevels(group)
      group.name<-levels(group);
      
      # Change the variables of classe (the groups and classes) into factor:
      if (!is.factor(classe[, 1]))  classe[, 1] <- as.factor(classe[, 1])
      if (!is.factor(classe[, 2]))  classe[, 2] <- as.factor(classe[, 2])
      nb.classe <- nlevels(classe[, 2])
      classe.name<-levels(classe[, 2]);
      
      # Check that the levels of x[, q+1] are exactly the levels of classe[, 1]
      if (!identical(levels(classe[, 1]),levels(x[, q+1]))) {
        stop("The names of the groups in the two data frames (x and classe arguments) are different.")
      }
      
      # Add the classes to x, as the (q+2)th column:
      x <- data.frame(x, classe = factor(NA, levels = levels(classe[, 2])), stringsAsFactors = TRUE)
      for (n.x in levels(classe[, 1]))
        x[x[, q+1] == n.x, "classe"] <- classe[classe[, 1] == n.x, 2]
      
      if (misclass.ratio)
        x.pred <- x   else {
          x.pred <- x[is.na(x[, q+2]), 1:(q+1)]
          x.pred[, q+1] <- as.factor(as.character(x.pred[, q+1]))
        }
      group.pred<-x.pred[, q+1]
      nb.pred <- nlevels(group.pred)
      pred.name <- levels(group.pred)
      
      # Tables of frequences per group
      tabgr <- by(x[1:q], x[, q+1], table)
      
      # Means and variances/covariances per group for which the classe is unknown:
      tabgr.pred <- by(x.pred[1:q], x.pred[, q+1], table)
      
      # Tables of frequences per class
      tabcl <- by(x[1:q], x[, q+2], table)
      
      distances <- matrix(nrow = nb.pred, ncol = nb.classe,
                          dimnames = list(pred.name, classe.name))
      
      switch(crit,
             "1" = { 
               
               for (n.x in rownames(distances))  if (sum(x[, q+1] == n.x) > 0)  {
                 for (n.cl in colnames(distances))  {
                   # Computation of the group-classe distances:
                   distances[n.x, n.cl] <- switch(distance,
                                                  l1 = ddlppar(tabgr[[n.x]], tabcl[[n.cl]], p = 1),
                                                  l2 = ddlppar(tabgr[[n.x]], tabcl[[n.cl]], p = 2),
                                                  chisqsym = ddchisqsympar(tabgr[[n.x]], tabcl[[n.cl]]),
                                                  hellinger = ddhellingerpar(tabgr[[n.x]], tabcl[[n.cl]]),
                                                  jeffreys = ddjeffreyspar(tabgr[[n.x]], tabcl[[n.cl]]),
                                                  jensen = ddjensenpar(tabgr[[n.x]], tabcl[[n.cl]]),
                                                  lp = ddlppar(tabgr[[n.x]], tabcl[[n.cl]], p = p)
                   )
                 }
               }
             },
             "2" = {
               for (n.x in rownames(distances))  {
                 if (sum(x[, q+1] == n.x) > 0) {
                   for (n.cl in colnames(distances))  {
                     # numbers of the rows of x containing observations of group n.x
                     indcl <- which(x[, q+2] == n.cl)
                     # Number of observations in class n.cl
                     Tj <- length(unique(x[indcl, q+1]))
                     # The groups which belong to class n.cl
                     x.cl <- unique(x[indcl, q+1])
                     tabcl.tmp <- Reduce("+", tabgr[x.cl])/Tj
                     
                     distances[n.x, n.cl] <- switch(distance,
                                                    l1 = ddlppar(tabgr[[n.x]], tabcl.tmp, p = 1),
                                                    l2 = ddlppar(tabgr[[n.x]], tabcl.tmp, p = 2),
                                                    chisqsym = ddchisqsympar(tabgr[[n.x]], tabcl.tmp),
                                                    hellinger = ddhellingerpar(tabgr[[n.x]], tabcl.tmp),
                                                    jeffreys = ddjeffreyspar(tabgr[[n.x]], tabcl.tmp),
                                                    jensen = ddjensenpar(tabgr[[n.x]], tabcl.tmp),
                                                    lp = ddlppar(tabgr[[n.x]], tabcl.tmp, p = p)
                     )
                   }
                 }
               }
             }
      )
    } else {
      
      # Check if all elements of the list xf are arrays with the same dimnames
      nomdim <- lapply(xf, dimnames)
      identdim <- sapply(nomdim, function(x) identical(x, nomdim[[1]]))
      if (!all(identdim))
        stop("If xf is a list of arrays, all its components must have the same dimensions and dimension names.")
      
      # Check if the elements of tab are arrays with non-negative elements
      is.negative <- sapply(xf, function(x) any(x < 0))
      if (any(is.negative)) {
        stop("All elements of xf must be arrays with non negative elements.")
      }
      
      classe <- class.var[c("group", "class")]
      
      # Tables of frequences per group
      tabgr <- xf
      
      distances <- matrix(nrow = nrow(class.var), ncol = nlevels(class.var$class),
                          dimnames = list(as.character(class.var$group), levels(class.var$class)))
      switch(crit,
             "1" = {
               for (n.x in rownames(distances))  {
                 for (n.cl in colnames(distances))  {
                   # The groups which belong to class n.cl
                   x.cl <- as.character(class.var[class.var$class == n.cl, "group"])
                   # numbers of the elements of tabgr containing observations of class n.cl
                   indcl <- which(names(tabgr) %in% x.cl)
                   # Number of observations in class n.cl: sum of the tables corresponding to groups of class n.cl
                   Tj <- Reduce(sum, tabgr[indcl])
                   # Weighted mean frequencies of the class
                   tabcl.tmp <- Reduce("+", lapply(tabgr[indcl], function(x) { x*sum(x) }))/Tj
                   
                   distances[n.x, n.cl] <- switch(distance,
                                                  l1 = ddlppar(tabgr[[n.x]], tabcl.tmp, p = 1),
                                                  l2 = ddlppar(tabgr[[n.x]], tabcl.tmp, p = 2),
                                                  chisqsym = ddchisqsympar(tabgr[[n.x]], tabcl.tmp),
                                                  hellinger = ddhellingerpar(tabgr[[n.x]], tabcl.tmp),
                                                  jeffreys = ddjeffreyspar(tabgr[[n.x]], tabcl.tmp),
                                                  jensen = ddjensenpar(tabgr[[n.x]], tabcl.tmp),
                                                  lp = ddlppar(tabgr[[n.x]], tabcl.tmp, p = p)
                   )
                 }
               }
             },
             "2" = {
               for (n.x in rownames(distances))  {
                 for (n.cl in colnames(distances))  {
                   # The groups which belong to class n.cl
                   x.cl <- as.character(class.var[class.var$class == n.cl, "group"])
                   # numbers of the elements of tabgr containing observations of class n.cl
                   indcl <- which(names(tabgr) %in% x.cl)
                   # Number of groups in class n.cl
                   Tj <- length(indcl)
                   # Mean frequencies
                   tabcl.tmp <- Reduce("+", tabgr[indcl])/Tj
                   
                   distances[n.x, n.cl] <- switch(distance,
                                                  l1 = ddlppar(tabgr[[n.x]], tabcl.tmp, p = 1),
                                                  l2 = ddlppar(tabgr[[n.x]], tabcl.tmp, p = 2),
                                                  chisqsym = ddchisqsympar(tabgr[[n.x]], tabcl.tmp),
                                                  hellinger = ddhellingerpar(tabgr[[n.x]], tabcl.tmp),
                                                  jeffreys = ddjeffreyspar(tabgr[[n.x]], tabcl.tmp),
                                                  jensen = ddjensenpar(tabgr[[n.x]], tabcl.tmp),
                                                  lp = ddlppar(tabgr[[n.x]], tabcl.tmp, p = p)
                   )
                 }
               }
             }
      )
      
    }
    
    group.calc <- unlist(apply(distances, 1, which.min))
    names(group.calc) <- unlist(lapply(strsplit(names(group.calc), ".", fixed = TRUE), head, 1))
    
    prediction <- rbind(classe[is.na(classe[, 2]), ], classe[!is.na(classe[, 2]), ])
    prediction <- data.frame(prediction, NA, stringsAsFactors = TRUE)
    colnames(prediction)[2:3] <- c("prior.class", "predicted.class")
    rownames(prediction) <- prediction[, 1]
    lev.class <- levels(classe[, 2])
    prediction$predicted.class <- factor(NA, levels = lev.class)
    prediction[names(group.calc), "predicted.class"] <- lev.class[group.calc]
    
    # Changing of distances into proximities
    inv.distances <- 1/(distances)
    sum.inv.distances <- rowSums(inv.distances)
    prox <- inv.distances/sum.inv.distances
    
    if (distance == "lp")
      distance <- paste("lp with p =", p)
    
    results <- list(d = distance,
                    prediction = prediction,
                    distances = distances,
                    proximities = prox*100)
    if (misclass.ratio) {
      # Allocations per class:
      confusion.mat <- table(prediction[, 2:3])
      
      # Misclassification percents per class
      class.ratio <- diag(confusion.mat)/rowSums(confusion.mat)
      class.ratio <- class.ratio*100
      misclassed <- sum(prediction[, 2] == prediction[, 3], na.rm = TRUE)/sum(!is.na(prediction[, 2]))
      results <- c(results,
                   list(confusion.mat = confusion.mat,
                        misclassed = misclassed))
    }
    
    class(results) <- "discdd.predict"
    
    return(results)
  }
