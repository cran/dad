matddlp <-
function(x, p = 1)  {
  if (!is.folder(x))
    stop("x must be an object of class 'folder'.")

# Computing of the distances  
  distances = diag(0, nrow = length(x))
  dimnames(distances) = list(names(x), names(x))

  for (i in 2:length(x))  for (j in 1:(i-1))  {
      distances[i, j] = distances[j, i] = ddlp(x[[i]], x[[j]], p = p)
    }
  as.dist(distances)
}
