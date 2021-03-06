matddjensen <-
function(x)  {
  if (!is.folder(x))
    stop("x must be an object of class 'folder'.")

# Computing of the distances  
  distances = diag(0, nrow = length(x))
  dimnames(distances) = list(names(x), names(x))

  for (i in 2:length(x))  for (j in 1:(i-1))  {
      distances[i, j] = distances[j, i] = ddjensen(x[[i]], x[[j]])
    }
  as.dist(distances)
}
