as.data.frame.foldert <- function(x, row.names = NULL, optional = FALSE, ..., group.name = "time") {
  class(x) <- "folder"
  if (!attr(x, "same.cols"))
    stop("x must be a 'foldert' with the same column names.")
  
  if (!attr(x, "same.rows"))
    stop("x must be a 'foldert' with the same row names.")
  
  datf <- as.data.frame(x, row.names = NULL, optional = FALSE, ..., group.name = group.name)
  
  if ("Date" %in% class(attr(x, "times")))
    datf[, group.name] <- as.Date(datf[, group.name])
  if (is.ordered(attr(x, "times")))
    datf[, group.name] <- ordered(datf[, group.name], levels = levels(attr(x, "times")))
  if (is.numeric(attr(x, "times")))
    datf[, group.name] <- as.numeric(as.character(datf[, group.name]))
  
  return(datf)
}
