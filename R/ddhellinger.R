ddhellinger <- function(x1, x2) {
  # Hellinger metric
  
  # If x1 and/or x2 are vectors, change them into data frames with 1 column
  if (is.vector(x1))
    x1 <- data.frame(x = x1)
  if (is.vector(x2))
    x2 <- data.frame(x = x2)
  
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
  
  for (j in 1:ncol(x1)) {
    # If necessary, change the column into a factor
    if (!is.factor(x1[, k]))
      x1[, k] <- as.factor(x1[, k])
    if (!is.factor(x2[, k]))
      x2[, k] <- as.factor(x2[, k])
    # x1[, k] and x2[, k] must be factors with the same levels
    lev <- union(levels(x1[, k]), levels(x2[, k]))
    x1[, k] <- factor(as.character(x1[, k]), levels = lev)
    x2[, k] <- factor(as.character(x2[, k]), levels = lev)
  }
  
  # Table of joint probabilities of the 1st data set
  p1 <- table(x1)/nrow(x1)
  # Table of joint probabilities of the 2nd data set
  p2 <- table(x2)/nrow(x2)
  
  return(ddhellingerpar(p1, p2))
}
