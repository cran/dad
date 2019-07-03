matdistl2dnorm <-
function(x, method = "gaussiand", varwL = NULL)  {
  if (is.data.frame(x))
    stop("matdistl2d now applies to an object of class 'folder'.\nNotice that for versions earlier than 3.1, matdistl2d apply to a data frame'.")
  if (!is.folder(x))
    stop("x must be an object of class 'folder'.")
  if (any(!apply(x[[1]], 2, is.numeric)))
    stop("Non numeric column(s) in the data frames in x.")
  
  n <- length(x)

# Computing of the inner products
  W = diag(0, nrow = n)
  dimnames(W) = list(names(x), names(x))
  W = matipl2d(x, method = method, varwL = varwL)

  norme <- sqrt(diag(W))
  for (i in 2:n) for (j in 1:i)
    W[i, j] <- W[j, i] <- W[i, j]/(norme[i]*norme[j])

# Computing of the distances  
  distances = diag(0, nrow = length(x))
  dimnames(distances) = list(names(x), names(x))

  for (i in 2:n)  for (j in 1:(i-1))  {
      distances[i, j] = distances[j, i] = sqrt( 2 - 2 * W[i, j] )
    }
  as.dist(distances)
}
