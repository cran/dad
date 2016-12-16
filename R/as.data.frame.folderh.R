#as.data.frame.folderh <- function(x, row.names = NULL, optional = FALSE, ...) {
as.data.frame.folderh <- function(x, row.names = NULL, optional = FALSE, ..., elt = names(x)[2], key = attr(x, "keys")[1]) {
  
  fold <- as.folder.folderh(x, elt = elt, key = key)
  
  # Change the object of class "folder"  into a data frame
  x <- as.data.frame(fold, group.name = key)
  
  return(x)
}
