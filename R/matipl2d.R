matipl2d <-
function(x, method = "gaussiand", varwL = NULL)  {
  lot = x[, ncol(x)]
  x = x[-ncol(x)]
  W = diag(0, nrow = nlevels(lot))
  dimnames(W) = list(levels(lot), levels(lot))
  
  if ((method == "gaussiand")|(is.null(varwL)))
    {xi = x[lot == levels(lot)[1], ]
     W[1, 1] = l2d(xi, xi, method=method, varw1=NULL, varw2=NULL)
     for (i in 2:nlevels(lot))  
      {xi = x[lot == levels(lot)[i], ]
      W[i, i] = l2d(xi, xi, method=method, varw1=NULL, varw2=NULL)
      for (j in 1:(i-1))  
        {xj = x[lot == levels(lot)[j], ]
        W[i, j] = W[j, i] = l2d(xi, xj, method=method, varw1=NULL, varw2=NULL)
      }
    }
  } else
    {xi = x[lot == levels(lot)[1], ]
     W[1, 1] = l2d(xi, xi, method=method, varw1=varwL[[1]], varw2=varwL[[1]])
     for (i in 2:nlevels(lot))  
      {xi = x[lot == levels(lot)[i], ]
      W[i, i] = l2d(xi, xi, method=method, varw1=varwL[[i]], varw2=varwL[[i]])
      for (j in 1:(i-1))  
        {xj = x[lot == levels(lot)[j], ]
        W[i, j] = W[j, i] = l2d(xi, xj, method=method, varw1=varwL[[i]], varw2=varwL[[j]])
      }
    }
  }
  W
}
