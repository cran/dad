matdistl2d <-
function(x, method = "gaussiand", varwL = NULL)  {
  if (is.data.frame(x))
    stop("matdistl2d() now applies to an object of class 'folder'.")
  if (!is.folder(x))
    stop("x must be an object of class 'folder'.")
  if (any(!apply(x[[1]], 2, is.numeric)))
    stop("Non numeric column(s) in the data frames in x.")

#  lot = x[, ncol(x)]
#  x = x[-ncol(x)]
#  distances = diag(0, nrow = nlevels(lot))
#  dimnames(distances) = list(levels(lot), levels(lot))
  distances = diag(0, nrow = length(x))
  dimnames(distances) = list(names(x), names(x))
  if ((method == "gaussiand")|(is.null(varwL))) {
    for (i in 2:length(x))  for (j in 1:(i-1))  {
#      i.lot = which(lot == levels(lot)[i])
#      j.lot = which(lot == levels(lot)[j])
      distances[i, j] = distances[j, i] = distl2d(x[[i]], x[[j]], method=method)
    }
  } else {
    for (i in 2:length(x))  for (j in 1:(i-1))  {
#      i.lot = which(lot == levels(lot)[i])
#      j.lot = which(lot == levels(lot)[j])
      distances[i, j] = distances[j, i] = distl2d(x[[i]], x[[j]], method=method, varw1=varwL[[i]], varw2=varwL[[j]])
    }
  }
  as.dist(distances)
}
