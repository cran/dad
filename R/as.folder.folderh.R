#as.folder.folderh <- function(x, ..., which.elt = attr(x, "keys")[1:2]) {        
as.folder.folderh <- function(x, ..., elt = names(x)[2], key = attr(x, "keys")[1]) {
  # Change an object x of class 'folderh' into an object of class 'folder'.
  # Each element of this 'folder' will be the lines of x corresponding to
  # a group, and the elements of the other columns of g.
  
  keys <- attr(x, "keys")
  k <- which(keys == key); j <- which(names(x) == elt)
  
  if (k >= j)
    stop(paste("If", elt , " is the", j, "-th element of the folderh, then the key must be \n strictly before the", j, "-th element of attr(x, 'keys') (See ?as.folder.folderh)", sep=""))
  
#  datf <- x[[k]]
#  i <- k
#  while (i<j){
#    i <- i+1
#   datf <- merge(datf, x[[i]], by = keys[i-1])
#  }
#  fold <- as.folder(datf, groups = key)
  
   datf <- x[[j]]
  i <- j
  while (i>k){
    i <- i-1
    datf <- merge(x[[i]], datf, by = keys[i])
  }
  fold <- as.folder(datf, groups = key)
  
  
  return(fold)
}
