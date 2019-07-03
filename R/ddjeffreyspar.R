ddjeffreyspar <- function(p1, p2) {
  # Jeffreys divergence
  
  k <- length(dim(p1))
  if (length(dim(p2)) != k)
    stop("p1 and p2 must have the same number of dimensions.")
  
  if (k == 1) {
    # If univariate
    if (length(p1) != length(p2))
      stop("p1 and p2 must be vectors with the same length.")
    dd <- (p1 - p2)*log(p1/p2)
    dd[is.nan(dd)] <- NA
    return(sum(dd, na.rm = TRUE))
  } else {
    # Multivariate
    dd <- (as.numeric(p1) - as.numeric(p2))*log(as.numeric(p1)/as.numeric(p2))
    dd[is.nan(dd)] <- NA
    return(sum(dd, na.rm = TRUE))
  }
}
