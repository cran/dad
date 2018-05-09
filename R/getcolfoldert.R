getcol.foldert <- function(object, name) {
  if (!is.foldert(object))
    stop("getcol.foldert applies to an object of class 'foldert'.")
  
  xf <- lapply(object, "[", name)
  attributes(xf) <- attributes(object)
  
  return(xf)
}
