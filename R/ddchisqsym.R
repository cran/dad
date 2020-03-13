ddchisqsym <- function(x1, x2) {
  # Symmetrized Pearson Chi^2-distance
  
  # If x1 and/or x2 are vectors, change them into data frames with 1 column
  if (is.vector(x1))
    x1 <- data.frame(x = x1, stringsAsFactors = TRUE)
  if (is.vector(x2))
    x2 <- data.frame(x = x2, stringsAsFactors = TRUE)
  
  if (!is.data.frame(x1))
    stop("x1 must be a data frame or a vector.")
  if (!is.data.frame(x2))
    stop("x2 must be a data frame or a vector.")
  
  # Check if x1 and x2 have the same number of columns
  k <- ncol(x1)
  if (ncol(x2) != k)
    stop("ncol(x1) != ncol(x2)")
  
  # Do x1 and x2 have the same column names?
  if (!identical(colnames(x1), colnames(x2)))
    warning("x1 and x2 do not have the same column names.")
  
  x1 <- as.data.frame(x1, stringsAsFactors = TRUE)
  x2 <- as.data.frame(x2, stringsAsFactors = TRUE)
  
  for (j in 1:ncol(x1)) {
    # If necessary, change the column into a factor
    if (!is.factor(x1[, j]))
      x1[, j] <- as.factor(x1[, j])
    if (!is.factor(x2[, j]))
      x2[, j] <- as.factor(x2[, j])
    # x1[, j] and x2[, j] must be factors with the same levels
    lev <- union(levels(x1[, j]), levels(x2[, j]))
    x1[, j] <- factor(as.character(x1[, j]), levels = lev)
    x2[, j] <- factor(as.character(x2[, j]), levels = lev)
  }
  
  # Table of joint probabilities of the 1st data set
  p1 <- table(x1)/nrow(x1)
  # Table of joint probabilities of the 2nd data set
  p2 <- table(x2)/nrow(x2)
  
  return(ddchisqsympar(p1, p2))
}
