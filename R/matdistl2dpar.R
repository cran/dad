matdistl2dpar <-
function(meanL, varL)
{
# Computing of the inner products
  W = diag(0, nrow = length(meanL))
  dimnames(W) = list(names(meanL), names(meanL))
  W = matipl2dpar(meanL, varL)

# Computing of the distances  
  distances = diag(0, nrow = length(meanL))
  dimnames(distances) = list(names(meanL), names(meanL))

  for (i in 2:length(meanL))  for (j in 1:(i-1))  {
      distances[i, j] = distances[j, i] = sqrt( W[i, i] + W[j, j] - 2 * W[i, j] )
    }
  as.dist(distances)
}
