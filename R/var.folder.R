var.folder <- function(x, na.rm = FALSE, use = "everything") {
  # 'var' method for objects of class 'folder': var per data frame.
  # - x: object of class 'folder'.
  # - use:  see 'var()'. Default: "everything".
  
  fold <- x
  
  # Check the arguments
  if (!attr(fold, "same.cols"))
    stop("'var.folder' cannot be applied to a folder with different column names.")
  
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
  
  return(lapply(fold.num, var, na.rm = na.rm, use = use))
}
