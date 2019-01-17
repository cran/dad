matwasserstein <-
function(x)  {
  if (!is.folder(x))
    stop("x must be an object of class 'folder'.")
  if (any(!apply(x[[1]], 2, is.numeric)))
    stop("Non numeric column(s) in the data frames in x.")

  distances = diag(0, nrow = length(x))
  dimnames(distances) = list(names(x), names(x))
  for (i in 2:length(x))  for (j in 1:(i-1))  {
    distances[i, j] = distances[j, i] = wasserstein(x[[i]], x[[j]])
  }
  as.dist(distances)
}
