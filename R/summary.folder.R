summary.folder <- function(object, ...){
  summ.fold <- lapply(object, summary, ...)
  attr(summ.fold, "same.cols") <- attr(object, "same.cols")
  attr(summ.fold, "same.rows") <- attr(object, "same.rows")
#  class(summ.fold) <- "summary.folder"
  return(summ.fold)
}
  