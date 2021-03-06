as.data.frame.folder <- function(x, row.names = NULL, optional = FALSE, ..., group.name = "group") {
  
  fold <- x
  name.fold <- deparse(substitute(x))
  
  # Check of the arguments
  if (!is.folder(fold))
    stop(paste(name.fold, "is not of class 'folder'."))

  # The grouping variable (given by the names of the elements of the folder)
  g <- names(fold)
  
  # Building of the data frame (the grouping variable on the last column):
  # - the first group
  x <- data.frame(fold[[1]], group = g[1], stringsAsFactors = TRUE)
  # - and the next groups
  if (length(g) > 1) {
    for (n in 2:length(g)) {
      x <- rbind(x, data.frame(fold[[n]], group = g[n], stringsAsFactors = TRUE))
    }
  }
  colnames(x)[ncol(x)] <- group.name
  
  return(x)
}
