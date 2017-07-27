mtgorder <- function(x, classes = "all", display = FALSE) {
  # Branching order of the vertices belonging to the classes defined
  # by the classes argument.
  # - x: object of class 'foldermtg'
  # - classes: character vector
  
  # Control: is x of class 'foldermtg'?
  if (!is.foldermtg(x))
    stop("mtgorder applies to an object of class 'foldermtg'.")
    
  if (classes != "all") {
    if (!(classes %in% x$classes[x$classes$SCALE > 0, "SYMBOL"])) {
      stop("'classes' argument does not match with the vertex classes defined in x.")
    }
  }

  # If classes=="all": creating the list of all classes found in x
  if (classes[1] == "all")
    classes <- as.character(x$classes[x$classes$SCALE > 0, "SYMBOL"])
    
  # v.order is the vector containing the branching order of each vertex
  # that is the index of the column where this vertex is
  topo <- x$topology
  mat.order <- apply(topo[, -ncol(topo)], 1, nchar) > 0
  v.order <- apply(mat.order, 2, which)
  
  for (cl in classes) {
    x[[cl]] <- data.frame(x[[cl]], Order = v.order[rownames(x[[cl]])])
  }
  
  if (display)
    print(x[classes])

  return(invisible(x))
}
