ddlppar <- function(p1, p2, p = 1) {
  # L^p distance
  
  k <- length(dim(p1))
  if (length(dim(p2)) != k)
    stop("p1 and p2 must have the same number of dimensions.")
  
  if (k == 1) {
    # If univariate
    if (length(p1) != length(p2))
      stop("p1 and p2 must be vectors with the same length.")
    dd <- sum(abs(p1 - p2)^p)
    return(dd^(1/p))
  } else {
    # Multivariate
    dd <- sum(abs(as.numeric(p1) - as.numeric(p2))^p)
    return(dd^(1/p))
  }
}
