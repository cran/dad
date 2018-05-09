rmcol.foldert <- function(object, name) {
  if (!is.foldert(object))
    stop("rmcol.foldert applies to an object of class 'foldert'.")
  
  xf <- lapply(object, function(x)x[!(names(x) %in% name)])
  attributes(xf) <- attributes(object)
  
  return(xf)
}
