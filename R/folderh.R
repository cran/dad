folderh <- function(df1, key1, df2, ..., na.rm = TRUE) {
  # df1 :  a data frame with at least 2 columns.
  # df2 :  a data frame.
  #        If df1 is a list, df2 must be NULL.
  # keys  : string vector.
  #        - If df1 and df2 are data frames, keys is a single
  #          character string, and it is the name of the column of g
  #          and x which contains the keys.
  #        - If df1 is a list and df2 = NULL, keys is a vector of
  #          strings, and length(keys) = length(df1)-1
  #          and the keys[k] is the name of a column of df1[[k]] and
  #          also the name of a column of df1[[k+1]]. It is the
  #          keys: the name of the variable which links each line of
  #          df1[[k]] to some lines of df1[[k+1]].
  #        - If df1 and df2 are data frames, and ... contains one or
  #          more data frames, then a list of the data frames:
  #          c(list(df1, df2), as.list(...)) is built, and we do the
  #          same as when df1 is a list of data frames.
  # ... :  if df1 and df2 are data frames, it can be one or more
  #        keys and data frames, ordered so: key2, df3, key3, df4...
  #
  # The object returned will be of class: "folderh".
  #
  # An objet of class "folderh" is a list of 2 or more data frames:
  # - [[1]]: data frame of at least 2 columns. One of them, whose
  #          name is the value of "key[1]" argument, contains the
  #          groups (df1 argument).
  # - [[2]]: data frame of (p+1) columns. The name of one column is
  #          the value of "key2" argument, and this column contains
  #          the groups. The other columns contain the data
  #          (df argument).
  # - and so on.
  #
  # An object of class "folderh" has one attribute named "keys"
  # - If there are only two data frames df1 and df2: key1.
  # - If there are more than two data frames: c(key1, key2, ...).

  # get the names of the arguments
  name.1 <- deparse(substitute(df1))
  name.2 <- deparse(substitute(df2))
  name.key <- deparse(substitute(key1))
  matchcall <- as.character(match.call()[-1])
  name.dots <- matchcall[! matchcall %in% c(name.1, name.2, name.key, key1, "na.rm")]
  dots <- list(...)
  
  # Names of the first and third arguments
  name.df <- c(name.1, name.2)

  # Initialisation of the list: list of the two first data frames and key of the relation
  X <- list(df1, df2)
  names(X) <- name.df
  keys <- key1

  # Add supplementary elements to lists of data frames and keys
  if (length(dots) > 0)
   {
    # Add the names of supplementary data frames to the names of the elements of the folderh
    name.X.supp <- name.dots[seq(2, length(dots), by = 2)]
    name.df <- c(name.df, name.X.supp)

    # Add the supplementary data frames to the list
    X.supp <- dots[seq(2, length(dots), by = 2)]
    X <- c(X, X.supp)
    names(X) <- name.df

    # The keys
    name.keys.supp <- name.dots[seq(1, length(dots), by = 2)]
    name.key <- c(name.key, name.keys.supp)
    keys.supp <- dots[seq(1, length(dots), by = 2)]
    keys <- c(list(keys), keys.supp)
   }

  # Test if all elements of the list are data frames:
  is.df <- sapply(X, is.data.frame)
  if (!prod(is.df))
    stop(paste("Argument(s)", name.df[!is.df],
          "is/are no data frame(s)."))
          
  # Test if all data frames have at least 2 columns
  ncolX <- sapply(X, ncol)
  less2col <- sapply(ncolX, "<", 2)
  if (any(less2col))
    stop("The data frame arguments must have at least 2 columns.")

  # Test if all keys are atomic
  is.atom <- sapply(keys, is.atomic)
  if (!prod(is.atom))
    stop(paste("Argument(s)", name.keys.supp[!is.atom],
          "is/are no atomic.\n"))
  lgkeys <- sapply(keys, length)
  if (any(lgkeys > 1))
    keys <- lapply(keys, head, 1)
  keys <- unlist(keys)
  
  # Is each key the name of a column in the two corresponding adjacent data frames
  for (k in 1:length(keys))
   {
    keyg <- keys[k]
    dfg1 <- X[[k]]
    dfg2 <- X[[k+1]]
    # Is keyg the name of a column of dfg1 data frame?
    if (! keyg %in% colnames(dfg1))
      stop(paste("There is no", keyg, "variable in", names(X)[k]))
      
    # Is keyg the name of a column of dfg2 data frame?
    if (! keyg %in% colnames(dfg2))
      stop(paste("There is no", keyg, "variable in", names(X)[k+1]))
      
    # In df1 data frame: check if each group in keyg occurs only once
    if (max(table(dfg1[, keyg])) > 1)
      stop(paste("A level of", names(X)[k], "[,", keyg, "] cannot occur more than once."))

    # If na.rm = TRUE: suppression of the lines for which dfg1[, keyg] is NA
    if (na.rm) {
      X[[k]] <- dfg1[!is.na(dfg1[, keyg]), ]
    }
  
    # If na.rm = TRUE: suppression of the lines for which dfg2[, keyg] is NA
    if (na.rm) {
      X[[k+1]] <- dfg2[!is.na(dfg2[, keyg]), ]
    }
   }
  
  # End of the tests
  
  
  # Folderh of the first two data frames (df1 and df2)
    keyg <- key1
  
    # Creation of the folderh
    foldh <- list(X[[1]], X[[2]])
    names(foldh) <- c(name.1,name.2)
    class(foldh) <- "folderh"
    attr(foldh, "keys") <- keyg

  # If there are more than 2 data frames: add the supplementary data frames
  if (length(X) > 2) 
   {
    for (k in 3:length(X))
     {
      # Append X[[k]] to the folderh
      Xk <- X[[k]]
      foldh <- appendtofolderh(fh = foldh, df = Xk, key = keys[k-1], after = TRUE)
     }
   }

  names(foldh) <- name.df

  return(foldh)
}
