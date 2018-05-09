print.foldert <- function(x, ...) {
  xprint <- list()
  for (tt in names(x))
  {
    if ((nrow(x[[tt]]) > 0)&(ncol(x[[tt]]) > 0))
    {
      isna <- apply(x[[tt]], 1, is.na)
      if (is.vector(isna))
      {
        whichna <- which(isna)
      } else {
        whichna <- which(apply(isna, 2, all))
      }
    } else {
      whichna <- numeric()
    }
    
    if (length(whichna) == 0)
    {
      xprint[[tt]] <- x[[tt]]
    } else {
      xprint[[tt]] <- x[[tt]][-whichna, , drop = FALSE]
    }
  }
  attr(xprint, "times") <- attr(x, "times")
  attr(xprint, "same.rows") <- attr(x, "same.rows")
  
  print(xprint, ...)
  
  return(invisible(x))
}