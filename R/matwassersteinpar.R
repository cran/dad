matwassersteinpar <-
function(meanL, varL)
{
  distances = diag(0, nrow = length(meanL))
  dimnames(distances) = list(names(meanL), names(meanL))
  for (i in 2:length(meanL))  
    {for (j in 1:(i-1))  
      {distances[i, j] = distances[j, i] = wassersteinpar(meanL[[i]], varL[[i]], meanL[[j]], varL[[j]])
      }
    }
  as.dist(distances)
}
