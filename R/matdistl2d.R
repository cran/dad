matdistl2d <-
function(x, method = "gaussiand", varwL = NULL)  {
  lot = x[, ncol(x)]
  x = x[-ncol(x)]
  distances = diag(0, nrow = nlevels(lot))
  dimnames(distances) = list(levels(lot), levels(lot))
  if ((method == "gaussiand")|(is.null(varwL))) {
    for (i in 2:nlevels(lot))  for (j in 1:(i-1))  {
      i.lot = which(lot == levels(lot)[i])
      j.lot = which(lot == levels(lot)[j])
      distances[i, j] = distances[j, i] = distl2d(x[i.lot, ], x[j.lot, ], method=method)
    }
  } else {
    for (i in 2:nlevels(lot))  for (j in 1:(i-1))  {
      i.lot = which(lot == levels(lot)[i])
      j.lot = which(lot == levels(lot)[j])
      distances[i, j] = distances[j, i] = distl2d(x[i.lot, ], x[j.lot, ], method=method, varw1=varwL[[i]], varw2=varwL[[j]])
    }
  }
  as.dist(distances)
}
