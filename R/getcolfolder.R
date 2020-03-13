getcol.folder <- function(object, name) {
  if (!is.folder(object))
    stop("getcol.folder applies to an object of class 'folder'.")
  
  xf <- lapply(object, "[", name)
  attributes(xf) <- attributes(object)
  
  return(xf)
}
