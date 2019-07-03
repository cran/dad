matddchisqsympar <-
function(freq)  {
  # Computing of the distances  
  distances = diag(0, nrow = length(freq))
  dimnames(distances) = list(names(freq), names(freq))

  for (i in 2:length(freq))  for (j in 1:(i-1))  {
      distances[i, j] = distances[j, i] = ddchisqsympar(freq[[i]], freq[[j]])
    }
  as.dist(distances)
}
