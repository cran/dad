skewness.folder <- function(x, na.rm = FALSE, type = 3) {
  # 'skewness' method for objects of class 'folder': skewness coefficient per column for each data frame.
  
  # Check the arguments
  if (!is.folder(x))
    stop("x must be an object of class 'folder'.")
  
  fold <- x
  
  x <- fold[[1]]
  jnum <- logical(ncol(x))
  for (j in 1:ncol(x)) {
    jnum[j] <- is.numeric(x[, j])
  }
  
  notnum <- colnames(x)[!jnum]
  if (length(notnum) > 0)
    warning(paste("There are omitted variables (non numeric):   ", paste(notnum, collapse = "   "), sep = ""), immediate. = TRUE)
  
  fold.num <- vector("list", length(fold))
  for (n in 1:length(fold.num))
    fold.num[[n]] <- fold[[n]][jnum]
  names(fold.num) <- names(fold)
  
  return(lapply(fold.num, function(x) apply(x, 2, skewness, na.rm = na.rm, type = type)))
}