as.data.frame.folderh <- function(x, row.names = NULL, optional = FALSE, ..., elt = names(x)[2], key = attr(x, "keys")[1]) {

  # Index of the key  
  keys <- attr(x, "keys")
  k <- which(keys == key) 
  # Index of the data frame 
  j <- which(names(x) == elt)
    # Checking the consistency of these indices and returning an error message if not
  if (k >= j)
    stop(paste("If", elt , " is the", j, "-th element of the folderh, then the key must be \n strictly before the", j, "-th element of attr(x, 'keys') (See ?as.folder.folderh)", sep=""))
   
  # datf: the data frame containing the data that will be returned
  datf <- x[[j]]
  cnames <- names(datf)
  
  if(j>k+1)
   {i <- j
    while (i>k+1){
      i <- i-1
      datf <- merge(datf, x[[i]], by = keys[i], sort = FALSE)
      # Suppression of the columns of x[[i]] which are not keys 
      cnames <- union(union(keys[i-1],keys[i]),cnames)
      datf <- datf[cnames]
    }
   }
  datf <- merge(datf, x[[k]], by = keys[k], sort = FALSE)
  
  return(datf)
}
