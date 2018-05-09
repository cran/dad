rmrow.folder <- function(object, name) {
  if (!is.folder(object))
    stop("rmrow.folder applies to an object of class 'folder'.")
  
  xf <- list()
  for (k in 1:length(object)) {
    objk <- object[[k]]
    xf[[k]] <- objk[!(rownames(objk) %in% name), ]
  }
  
  attributes(xf) <- attributes(object)
  
  return(xf)
}
