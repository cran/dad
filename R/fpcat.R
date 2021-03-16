fpcat <-
  function(xf, group.name="time", method = 1, ind = 1, nvar = NULL,
           gaussiand=TRUE, windowh=NULL, normed=TRUE, centered=TRUE, data.centered=FALSE,
           data.scaled=FALSE, common.variance=FALSE, nb.factors=3, nb.values=10,
           sub.title="", plot.eigen=TRUE, plot.score=FALSE, nscore=1:3, filename=NULL)
  {
    if (!(is.foldert(xf) | is.data.frame(xf))){
      stop("fpcat applies to a data frame or an object of class 'foldert'.")
    }
    
    if (is.data.frame(xf)) {
      xf <- as.foldert(xf, method = method, ind = ind, timecol = group.name, nvar = nvar)
      if(method == 2) group.name <- "time"
    }
    
    times <- attr(xf, "times")
    
    # Perform the FPCA, using fpcad()
    class(xf) <- "folder"
    results <- fpcad(xf, gaussiand=gaussiand, windowh=windowh,
                     normed=normed, centered=centered, data.centered=data.centered, data.scaled=data.scaled, 
                     common.variance=common.variance, nb.factors=nb.factors, nb.values=nb.values, sub.title=sub.title,
                     plot.eigen=plot.eigen, plot.score=plot.score, nscore=nscore, group.name=group.name, filename=filename)
    
    # Object of class "fpcat"
    results$call <- match.call()
    results <- append(results, list(times), after = 3) # (Add the times vector as the 4th element of the 'foldert')
    names(results)[4] <- "times"
    class(results) <- "fpcat"
    
    # Returning the list of results
    return(results)
  }
