as.data.frame.foldert <- function(x, row.names = NULL, optional = FALSE, ..., group.name = "time") {

  name.fold <- deparse(substitute(x))

  # Check of the arguments
  if (!is.foldert(x))
    stop(paste(name.fold, "is not of class 'foldert'."))

  class(x) <- "folder"
  
  datf <- as.data.frame(x, row.names = NULL, optional = FALSE, ..., group.name = group.name)
  
  if ("Date" %in% class(attr(x, "times")))
    datf[, group.name] <- as.Date(datf[, group.name])
  if (is.ordered(attr(x, "times")))
    datf[, group.name] <- ordered(datf[, group.name], levels = levels(attr(x, "times")))
  if (is.numeric(attr(x, "times")))
    datf[, group.name] <- as.numeric(as.character(datf[, group.name]))
  
  return(datf)
}
