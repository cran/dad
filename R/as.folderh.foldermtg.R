as.folderh.foldermtg <- function(x, classes) {
  # x: object of class 'foldermtg'
  # classes: vertex classes to be included in the returned folderh
  # na.rm: logical. If TRUE, then during the call of folderh(),
  #        the rows of each data frame for which the key is NA are removed.
  
  # Check if all elements of 'classes' are classes of the MTG
  wrongclasses <- (! classes %in% as.character(x$classes$SYMBOL))
  if (any(wrongclasses))
    stop(paste0("Element(s) of 'classes' which are not classes of x :  '",
         paste(classes[wrongclasses], collapse = "', '"), "'"))
  
  # Scales of the classes
  scales <- sapply(classes, function(classe){x$classes[x$classes$SYMBOL == classe, "SCALE"]})
  
  # Reorder the scales and classes in increasing order of scales
  order.scale <- order(scales)
  scales <- scales[order.scale]
  classes <- classes[order.scale]
  
  # Check if all scales are distinct
  isduplic <- duplicated(scales)
  if (any(isduplic))
    stop("The classes given by 'classes' argument must be at distinct scales")
  
  # Initialisation of the list (table of the 1st class of vertex)
  tab <- x[[classes[1]]]
  tab <- data.frame(rownames(tab), tab, stringsAsFactors = FALSE)
  colnames(tab)[1] <- classes[1]
  listdf <- list(tab)
  
  for (k in 1:(length(classes)-1)) {
    # The k-th class and the (k+1)-th scale:
    classe <- classes[k]
    scal <- scales[k+1]
    
    # Vertices of the k-th class
    vertices <- rownames(x[[classe]])
    
    # Table of features of the (k+1)-th class
    tab <- x[[names(scal)]]
    tab <- data.frame(character(nrow(tab)), rownames(tab), tab, stringsAsFactors = FALSE)
    colnames(tab)[1:2] <- c(classe, names(scal))
    
    # For each vertex of the k-class: components of scale scal
    for (vertex in vertices) {
      # - the components of scale scal
      comp <- mtgcomponents(x, vertex, scal)
      # - select the components which are of the wanted class,
      #   i.e. the components which match with rownames(tab)
      comp <- intersect(comp, rownames(tab))
      # - add the "parent" to the corresponding rows of tab
      tab[comp, 1] <- vertex
    }
    listdf[[names(scal)]] <- tab
  }
  
  # Change the 1st column of listdf[[1]] into a factor
  listdf[[1]][, 1] <- as.factor(listdf[[1]][, 1])
  # For each other data frame, change the 2 first columns into factors
  for (k in 2:length(listdf)) {
    listdf[[k]][, 1] <- as.factor(listdf[[k]][, 1])
    listdf[[k]][, 2] <- as.factor(listdf[[k]][, 2])
  }
  
  # Arguments to be passed to function folderh()
  argsfolderh <- c(listdf[1], list(classes[1]), listdf[2])
  names(argsfolderh) <- c("df1", "key1", "df2")
  if (length(classes) > 2){
    dots <- list()
    for (k in 3:length(classes))
      dots <- c(dots, list(classes[k-1]), listdf[k])
    argsfolderh <- c(argsfolderh, "..." = dots)
  }
  argsfolderh <- c(argsfolderh, na.rm = list(TRUE))
  
  # Building of the returned folderh
  foldh <- do.call(folderh, argsfolderh)
  names(foldh) <- classes
  names(attr(foldh,"keys")) = NULL
  
  # Returned: an object of class 'folderh'
  return(foldh)
}