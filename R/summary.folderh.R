summary.folderh <- function(object, ...){
  summ.foldh <- lapply(object, summary, ...)
  attr(summ.foldh, "keys") <- attr(object, "keys")
  class(summ.foldh) <- "summary.folderh"
  return(summ.foldh)
}
  