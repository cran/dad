rmrow.foldert <- function(object, name) {
  if (!is.foldert(object))
    stop("rmrow.foldert applies to an object of class 'foldert'.")
  
  xf <- list()
  for (k in 1:length(object)) {
    objk <- object[[k]]
    xf[[k]] <- objk[!(rownames(objk) %in% name), ]
  }
  
  attributes(xf) <- attributes(object)
  
  return(xf)
}
