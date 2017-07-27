mtgcomponents <- function(x, vertex, scale) {
  # x: object of class 'foldermtg'
  # vertex: character. Identifier of the vertex of which the decomposition is wanted
  # scale: numeric. Scale of the components to be returned
  
  # Data frame of the plant topology
  topo <- x$topology[-ncol(x$topology)]
  xvtx <- x$topology[, ncol(x$topology)]
  matorder <- apply(topo, 1, nchar) > 0
  vorder <- apply(matorder, 2, which)
  names(xvtx) <- rownames(topo)
  
  # Data frame of the classes:
  classes <- x$classes
  
  # To prevent further problems: change each column into characters
  # (otherwise, they would be factors)
  for (j in 1:ncol(topo)) topo[,j] <- as.character(topo[,j])
  
  # Test if vertex exists
  if (! vertex %in% rownames(topo))
    stop(paste0("No vertex is called ", vertex,"  (See the rownames of topology data frame)"))
    
  # Test if scale is higher than vertex's scale
  vertexclass <- substring(xvtx[vertex], 3, 3)
  vertexscale <- classes[classes$SYMBOL == vertexclass, "SCALE"]
  if (scale <= vertexscale)
    stop(paste0("scale = ", scale, " and the scale of ", vertex, " is ", vertexscale, ".\n  scale must be higher than vertex's scale"))
    
  # If vertex is on the last row of topo, it has no component:
  if (vertex == tail(rownames(topo), 1))
    return(character(0))
  
  # Line number of topo data frame containing vertex:
  ivertex <- which(rownames(topo) == vertex)
  # Column number of topo data frame containing vertex:
  # (i.e. index of the non-empty character string in topo[ivertex,])
  jvertex <- which(nchar(as.character(topo[ivertex, ])) > 0)
  
  # Vector of the components: initialisation
  components <- character(0)                               
  
  i <- ivertex+1
  j <- jvertex
  
  # If the symbol following 'vertex' does not start with "^/",
  # there is no decomposition of 'vertex'
  # and the returned components are character(0)
  if (substring(topo[i, jvertex], 1, 2) != "^/") {
    return(components)
  } else { # Otherwise, every components of 'vertex', of any scale, are researched:
    # The following is added to the vector components
    components <- c(components, rownames(topo)[i])
    
    if (i < nrow(topo)) {
      # Relationships between the vertices following the i-th vertex
      relprec <- substring(topo[-(1:i), jvertex], 1, 2)
      # Vertices which are connected to the previous by a "<" or "/"
      # relationship AND are not situated on a column left to vertex
      is.comp <- ((relprec %in% c("", "^<", "^/")) & (vorder[-(1:i)] >= jvertex))
      # Length of the sequence of successive TRUEs
      if (all(is.comp)) {
        lgseq <- length(is.comp)
      } else {
        lgseq <- min(which(!is.comp)) - 1
      }
      # If the length of this sequence is > 0, the vertices situated between
      # the i-th one and the end of the sequence are added
      if (lgseq > 0) {
        components <- c(components, rownames(topo)[i + (1:lgseq)])
      }
    }
  }
  
  # Among the components of vertex, are selected those whose scale
  # corresponds to 'scale' argument.
  
  # The class of each component:
  compsymb <- substring(xvtx[components], 3, 3)
  # Classes of scale corresponding to scale argument:
  scaleclass <- as.character(classes[classes$SCALE == scale, "SYMBOL"])
  
  # Returned: the components of the wanted scale:
  return(components[compsymb %in% scaleclass])
}