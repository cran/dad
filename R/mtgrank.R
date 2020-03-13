mtgrank <- function(x, classe, parent.class = NULL, sibling.classes = NULL,
  relative = FALSE, from = c("origin", "end"), rank.name = "Rank", display = FALSE) {

 # Controls 
 # on the argument x
 if (!is.foldermtg(x))
    stop("mtgrank applies to an object of class 'foldertg'.")
 # on the argument from
  from <- from[1]
  if (!from %in% c("origin", "end"))
    stop("from must be 'origin' or 'end'.")
 # on the arguments classe and parent.class
  classes <- x$classes
  scal <- classes[classes$SYMBOL == classe, "SCALE"]
  if (is.null(parent.class))
    parent.class <- as.character(classes[classes$SCALE == scal-1, "SYMBOL"])
  if (length(parent.class) > 1)
    stop("parent.class argument cannot be of length > 1.\nYou should indicate the parent class.")
 # on the argument sibling.classes
  if (!is.null(sibling.classes)) {
    if (any(classes[classes$SYMBOL %in% sibling.classes, "SCALE"] != scal))
      stop("Every class indicated by sibling class must be at the same scale as classe.")
  }
  
  x.original <- x
 
 # Controls if the branching order of the vertices are in the entity data frames 
 # corresponding to the arguments classe, parent.class, sibling.classes
  exist.orderparent <- ("Order" %in% colnames(x[[parent.class]]))
  exist.orderclass <- ("Order" %in% colnames(x[[classe]]))
  exist.ordersibling <- logical(length(sibling.classes))
  for (cl in sibling.classes) {
    exist.ordersibling[cl] <- ("Order" %in% colnames(x[[cl]]))
  }

 # Appending the branching order of the vertices to the entity data frames 
 # corresponding to the arguments classe, parent.class, sibling.classes   
  if (!exist.orderparent)
    x <- mtgorder(x, classes = parent.class)
  if (!exist.orderclass)
    x <- mtgorder(x, classes = classe)
  for (cl in sibling.classes)
    if (!exist.ordersibling[cl])
      x <- mtgorder(x, classes = cl)
    
  # identification of the data frames to be considered
  tabclass <- x[[classe]]
  tabparent <- x[[parent.class]]
  if (!is.null(sibling.classes)) tabsibl <- x[sibling.classes]
  v.class <- tabclass["Order"]
  v.class <- data.frame(v.class, parent = character(nrow(v.class)), stringsAsFactors = FALSE)
  v.parent <- tabparent["Order"]
  if (!is.null(sibling.classes)) {
    v.sibling <- data.frame(Order = numeric(0), stringsAsFactors = TRUE)
    for (cl in sibling.classes){
      v.sibling <- rbind(v.sibling, tabsibl[[cl]]["Order"])
    }
    v.sibling <- data.frame(v.sibling,
      parent = character(nrow(v.sibling)), stringsAsFactors = FALSE)
  }
  
  # Reorder the vertices in increasing order of the row names
  v.class <- v.class[sort(row.names(v.class)),,drop=FALSE ]
  v.parent <- v.parent[sort(row.names(v.parent)),,drop=FALSE ]
  if (!is.null(sibling.classes))
  v.sibling <- v.sibling[sort(row.names(v.sibling)),,drop=FALSE]
  
  # In each of these data frames, the parent vertex of each vertex
  for (v in rownames(v.class)) {
    ord <- v.class[v, "Order"]
    i.parent <- which((v.parent$Order == ord)&(rownames(v.parent) < v))
    v.class[v, "parent"] <- rownames(v.parent)[max(i.parent)]
  }

  if (!is.null(sibling.classes)) {
    for (v in rownames(v.sibling)) {
      ord <- v.sibling[v, "Order"]
      i.parent <- which((v.parent$Order == ord)&(rownames(v.parent) < v))
      v.sibling[v, "parent"] <- rownames(v.parent)[max(i.parent)]
    }
  }
  
  # Method for the computing of the ranks:
  # - "oa": from the origin, absolute ranks
  # - "ea": from the end of the sequence, absolute ranks
  # - "or": from the origin, relative ranks
  # - "er": from the end, relative ranks
  method <- substring(from, 1, 1)
  if (relative)
    method <- paste0(method, "r") else
    method <- paste0(method, "a")
  
  # Compute the ranks:
  # The considered class and the "sibling" classes in the same data frame:
  v.ranks <- data.frame(classe = "classe", v.class["parent"], stringsAsFactors = FALSE)
  if (!is.null(sibling.classes)) {
    v.ranks <- rbind(v.ranks,
      data.frame(classe = "sibling", v.sibling["parent"], stringsAsFactors = FALSE))}
  v.ranks <- data.frame(v.ranks, Rank = numeric(nrow(v.ranks)), stringsAsFactors = TRUE)
  v.ranks <- v.ranks[order(v.ranks$parent, row.names(v.ranks)), ]
  switch(method,
    oa = {
      for (vpa in unique(v.ranks$parent)) {
        v.ranks[v.ranks$parent == vpa, "Rank"] <- 1:sum(v.ranks$parent == vpa)
      }
    },
    or = {
      for (vpa in unique(v.ranks$parent)) {
        ranks <- 1:sum(v.ranks$parent == vpa)
        v.ranks[v.ranks$parent == vpa, "Rank"] <- (ranks - 1)/max(ranks)
      }
    },
    ea = {
      for (vpa in unique(v.ranks$parent)) {
        v.ranks[v.ranks$parent == vpa, "Rank"] <- sum(v.ranks$parent == vpa):1
      }
    },
    er = {
      for (vpa in unique(v.ranks$parent)) {
        ranks <- sum(v.ranks$parent == vpa):1
        v.ranks[v.ranks$parent == vpa, "Rank"] <- (ranks - 1)/max(ranks)
      }
    },
  )
  
  # Add the ranks to the element of the foldermtg corresponding to classe
  x[[classe]] <- data.frame(x[[classe]], Rank = v.ranks[row.names(x[[classe]]),"Rank"],
                            stringsAsFactors = TRUE)
  if (rank.name != "Rank"){
    j.rank <- which(colnames(x[[classe]]) == "Rank")
    colnames(x[[classe]][j.rank]) <- rank.name
  }
  
  # Remove "Order" column in x[[classe]], if it did not exist in x.original
  x[[classe]] <- x[[classe]][, union(names(x.original[[classe]]), "Rank")]
  x[[parent.class]] <- x.original[[parent.class]]
  x[sibling.classes] <- x.original[sibling.classes]
  
  # Display the result
  if (display) {
    cat(paste("\t", "absolute"[!relative], "relative"[relative], "ranks: from the", from, "\n\n"))
    print(x[classe])
  }
 
  return(invisible(x))
}
