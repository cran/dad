matdistl2dnormpar <-
function(meanL, varL)
{
  n <- length(meanL)
  # Computing of the inner products
  W = diag(0, nrow = n)
  dimnames(W) = list(names(meanL), names(meanL))
  W = matipl2dpar(meanL, varL)

  norme <- sqrt(diag(W))
  for (i in 2:n) for (j in 1:i)
    W[i, j] <- W[j, i] <- W[i, j]/(norme[i]*norme[j])
  
  # Computing of the distances  
  distances = diag(0, nrow = n)
  dimnames(distances) = list(names(meanL), names(meanL))

  
  for (i in 2:n)  for (j in 1:(i-1))  {
    distances[i, j] = distances[j, i] = sqrt( 2 - 2 * W[i, j] )
  }
  as.dist(distances)
}
