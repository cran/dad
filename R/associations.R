cramer.data.frame <- function(x, check = TRUE) {
  if (!is.data.frame(x))
    stop(deparse(substitute(x)), " is not a data.frame")
  x <- as.data.frame(x, stringsAsFactors = TRUE)
  if (check) {
    # Control and convert the variables to factors
    for (k1 in 1:ncol(x)) {
      if (!is.factor(x[, k1])){
        warning("x[, ", k1, "] is converted to a factor")
        x[, k1] = as.factor(x[, k1])
      }
    }
  }
  # Compute Cramer's V
  assoc <- matrix(nrow = ncol(x), ncol = ncol(x),
                  dimnames = list(colnames(x), colnames(x)))
  for (k1 in 1:ncol(x)) {
    x1 <- x[, k1]
    assoc[k1, k1] <- CramerV(x1, x1, conf.level = NA)
    for (k2 in k1:ncol(x)) {
      x2 <- x[, k2]
      assoc[k1, k2] <- assoc[k2, k1] <- CramerV(x1, x2, conf.level = NA)
    }
  }
  return(assoc)
}

tschuprow.data.frame <- function(x, check = TRUE) {
  if (!is.data.frame(x))
    stop(deparse(substitute(x)), " is not a data.frame")
  x <- as.data.frame(x, stringsAsFactors = TRUE)
  if (check) {
    # Control and convert the variables to factors
    for (k1 in 1:ncol(x)) {
      if (!is.factor(x[, k1])){
        warning("x[, ", k1, "] is converted to a factor")
        x[, k1] = as.factor(x[, k1])
      }
    }
  }
  # Compute Tschuprow's T
  assoc <- matrix(nrow = ncol(x), ncol = ncol(x),
                  dimnames = list(colnames(x), colnames(x)))
  for (k1 in 1:ncol(x)) {
    x1 <- x[, k1]
    assoc[k1, k1] <- TschuprowT(x1, x1)
    for (k2 in k1:ncol(x)) {
      x2 <- x[, k2]
      assoc[k1, k2] <- assoc[k2, k1] <- TschuprowT(x1, x2)
    }
  }
  return(assoc)
}

pearson.data.frame <- function(x, check = TRUE) {
  if (!is.data.frame(x))
    stop(deparse(substitute(x)), " is not a data.frame")
  x <- as.data.frame(x, stringsAsFactors = TRUE)
  if (check) {
    # Control and convert the variables to factors
    for (k1 in 1:ncol(x)) {
      if (!is.factor(x[, k1])){
        warning("x[, ", k1, "] is converted to a factor")
        x[, k1] = as.factor(x[, k1])
      }
    }
  }
  # Compute Pearson's contingency coefficient
  assoc <- matrix(nrow = ncol(x), ncol = ncol(x),
                  dimnames = list(colnames(x), colnames(x)))
  for (k1 in 1:ncol(x)) {
    x1 <- x[, k1]
    assoc[k1, k1] <- ContCoef(x1, x1, correct = FALSE)
    for (k2 in k1:ncol(x)) {
      x2 <- x[, k2]
      assoc[k1, k2] <- assoc[k2, k1] <- ContCoef(x1, x2, correct = FALSE)
    }
  }
  return(assoc)
}

phi.data.frame <- function(x, check = TRUE) {
  if (!is.data.frame(x))
    stop(deparse(substitute(x)), " is not a data.frame")
  x <- as.data.frame(x, stringsAsFactors = TRUE)
  if (check) {
    # Control and convert the variables to factors
    for (k1 in 1:ncol(x)) {
      if (!is.factor(x[, k1])){
        warning("x[, ", k1, "] is converted to a factor")
        x[, k1] = as.factor(x[, k1])
      }
    }
  }
  # Compute phi
  assoc <- matrix(nrow = ncol(x), ncol = ncol(x),
                  dimnames = list(colnames(x), colnames(x)))
  for (k1 in 1:ncol(x)) {
    x1 <- x[, k1]
    assoc[k1, k1] <- Phi(x1, x1)
    for (k2 in k1:ncol(x)) {
      x2 <- x[, k2]
      assoc[k1, k2] <- assoc[k2, k1] <- Phi(x1, x2)
    }
  }
  return(assoc)
}
