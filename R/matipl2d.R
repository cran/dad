matipl2d <-
function(x, method = "gaussiand", varwL = NULL)  {
  if (is.data.frame(x))
    stop("matipl2d() now applies to an object of class 'folder'.")
  if (!is.folder(x))
    stop("x must be an object of class 'folder'.")
  if (any(!apply(x[[1]], 2, is.numeric)))
    stop("Non numeric column(s) in the data frames in x.")
  
  W = diag(0, nrow = length(x))
  dimnames(W) = list(names(x), names(x))
  
  if ((method == "gaussiand")|(is.null(varwL))){
    W[1, 1] = l2d(x[[1]], x[[1]], method=method, varw1=NULL, varw2=NULL)
    for (i in 2:length(x)){
      W[i, i] = l2d(x[[i]], x[[i]], method=method, varw1=NULL, varw2=NULL)
      for (j in 1:(i-1)){
        W[i, j] = W[j, i] = l2d(x[[i]], x[[j]], method=method, varw1=NULL, varw2=NULL)
      }
    }
  } else {
    W[1, 1] = l2d(x[[1]], x[[1]], method=method, varw1=varwL[[1]], varw2=varwL[[1]])
    for (i in 2:length(x)){
      W[i, i] = l2d(x[[i]], x[[i]], method=method, varw1=varwL[[i]], varw2=varwL[[i]])
      for (j in 1:(i-1)){
        W[i, j] = W[j, i] = l2d(x[[i]], x[[j]], method=method, varw1=varwL[[i]], varw2=varwL[[j]])
      }
    }
  }
  
  W
}
