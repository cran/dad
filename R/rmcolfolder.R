rmcol.folder <- function(object, name) {
  if (!is.folder(object))
    stop("rmcol.folder applies to an object of class 'folder'.")
  
  xf <- lapply(object, function(x)x[!(colnames(x) %in% name)])
  attributes(xf) <- attributes(object)
  
  return(xf)
}
