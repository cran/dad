cut.data.frame <- function(x, breaks, labels = NULL, include.lowest = FALSE, right = TRUE,
                           dig.lab = 3L, ordered_result = FALSE, cutcol = NULL, ...) {
  # Applies cut() to each numeric column of a data frame.
  #
  # Arguments:
  # Arguments:
  #   - x      :         a data frame.
  #   - breaks :         list or numeric. It gives the breaks for each variable of the folder.
  #                      - If breaks is a list, its length is equal to the number of columns
  #                        in the data frame. It can be:
  #                          - a list of numeric vectors.
  #                            The j-th element corresponds to the column x[, j], and is a vector
  #                            of two or more unique cut points
  #                          - or a list of single numbers (greater or equal to 2).
  #                            Its j-th element gives the number of intervals into which th j-th variable
  #                            of the folder is to be cut.
  #                        The elements breaks[[j]] corresponding to non-numeric columns must be NULL.
  #                      - If breaks is a numeric vector, it gives the number of intervals into which
  #                        every columns x[, j] is to be cut.
  #   - labels :         list of character vectors. Its length is equal to the number of columns of x.
  #                      The j-th element gives the labels for the intervals of the j-th columns of the
  #                      data frame.
  #                      By default, labels are constructed using "(a,b]" interval notation.
  #                      If labels = FALSE, simple integer codes are returned instead of a factor.
  #   - include.lowest : logical, indicating if an 'x[i]' equal to the lowest (or highest, for right = FALSE) 'breaks' value should be included.
  #   - right          : logical, indicating if the intervals should be closed on the right (and open on the left) or vice versa.
  #   - dig.lab :      : integer which is used when labels are not given.
  #                      It determines the number of digits used in formatting the break numbers.
  #                      If it is a single value, it gives the number of digits for all variables of the folder.
  #                      If it is a list of integers, its length is equal to the number of variables,
  #                      and the j-th element gives the number of digits for the j-th variable of the folder.
  #   - ordered_result : logical: should the results be ordered factors?
  #   - cutcol         : numeric vector: numbers of the columns to be converted into factors.
  #                      These columns must all be numeric. Otherwise, there is a warning.
  
  # Number of variables
  p <- ncol(x)
  # Number of numerical columns
  colnum <- which(sapply(x, is.numeric))
  
  if (!is.null(cutcol)) {
    if (any(!(cutcol %in% 1:p)))
      stop("cutcol: some column numbers are wrong.")
    
    # Check if all columns given by cutcol are numeric
    notnum <- setdiff(cutcol, colnum)
    if (length(notnum) > 0) {
      warning("Non-numeric columns cannot be divided into intervals.")
      # Select numeric columns numbers in cutcol
      cutcol <- intersect(cutcol, colnum)
    }
  } else {
    # If cutcol is not given, cut() is applied on all numeric columns of x
    cutcol <- colnum
  }
  
  # Check if breaks is numeric or a list of numeric vectors
  
  if (is.list(breaks)){
    # If breaks is a list, labels must be a list with the same length
    if ((!is.null(labels))&(!is.list(labels)))
      stop("If breaks is a list, labels must also be a list.")
    if ((!is.null(cutcol))&(length(breaks) != length(cutcol)))
      stop("If breaks is a list, its length must be equal to the number of columns to be changed.")
    if ((!is.null(labels))&(length(labels) != length(cutcol)))
      stop("If labels is a list, its length must be equal to the number of columns to be changed.")
    
    # If breaks is a list, all of its elements must be numeric
    if (!all(sapply(breaks[cutcol], is.numeric))) {
      stop("breaks must be either numeric or a list of numeric vector")
    }
    # If the elements of breaks are not single values, are all these elements vectors
    # with 2 elements or more?
    nbr <- sapply(breaks, length)
    if (any(nbr != 1)) {
      if (any(nbr <= 2))
        stop("invalid number of intervals")
    }
  } else {
    if (!is.numeric(breaks))
      stop("breaks must be either numeric or a list of numeric vector")
    if ((!is.null(labels))&(!is.character(labels)))
      stop("labels: wrong value.")
    
    # If breaks is a numeric vector: change it in a list by duplicating it
    brk <- breaks
    breaks <- list()
    for (j in 1:p)
      breaks[[j]] <- brk
    
    # If labels is a character vector or a logical: change it in a list by duplicating it
    if (!is.null(labels)) {
      lab <- labels
      labels <- list()
      for (j in 1:p)
        labels[[j]] <- lab
    }
  }
  
  # Divide the columns of the data frame into intervals
  for (j in cutcol) {
    if (is.numeric(x[, j])) {
      x[, j] <- cut(x[, j], breaks = breaks[[j]], ordered_result = TRUE)
    }
  }
  return(x)
}
