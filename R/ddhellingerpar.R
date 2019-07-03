ddhellingerpar <- function(p1, p2) {
  # Hellinger metric
  
  k <- length(dim(p1))
  if (length(dim(p2)) != k)
    stop("p1 and p2 must have the same number of dimensions.")
  
  if (k == 1) {
    # If univariate
    if (length(p1) != length(p2))
      stop("p1 and p2 must be vectors with the same length.")
    dd <- sum((sqrt(p1) - sqrt(p2))^2)
    return(sqrt(dd))
  } else {
    # Multivariate
    dd <- sum((sqrt(as.numeric(p1)) - sqrt(as.numeric(p2)))^2)
    return(sqrt(dd))
  }
}
