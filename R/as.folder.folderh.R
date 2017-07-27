as.folder.folderh <- function(x, ..., elt = names(x)[2], key = attr(x, "keys")[1]) {
  # Change an object x of class 'folderh' into an object of class 'folder'.
  # Each element of this 'folder' will be the rows of x corresponding to
  # a group, and the elements of the other columns of g.
  
  # Change the object of class "folderh"  into a data frame
  dfx <- as.data.frame.folderh(x, elt = elt, key = key)

  fold  <- as.folder.data.frame(dfx, groups = key)

  return(fold)
}
