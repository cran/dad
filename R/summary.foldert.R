summary.foldert <- function(object, ...){
  summ.fold <- lapply(object, summary, ...)
  attr(summ.fold, "times") <- attr(object, "times")
  attr(summ.fold, "same.rows") <- attr(object, "same.rows")
  return(summ.fold)
}
  