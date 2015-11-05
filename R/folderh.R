folderh <- function(g, x, keyg = "rownames", keyx, nag.rm = TRUE, nax.rm = TRUE) {
  # g :    data frame with at least 1 column.
  # x :    data frame. The data.
  # keyg : string. the name of the column of g which contains the key.
  #        If omitted, the key is given by the rownames of g.
  # keyx : string. the name of the column of x which contains the key.
  #
  # The object returned will be of class: "folderh".
  #
  # An objet of class "folderh" is a list of two data frames names
  # "data" and "groups":
  # - df1: data frame of 2 columns. One of them, whose name is the value
  #        of "key1" argument, contains the groups (g argument).
  # - df2: data frame of (p+1) columns. The name of one column is the value of
  #        "key2" argument, and this column contains the groups. The other
  #        columns contain the data (x argument).
  #
  # An object of class "folderh" has one attribute named "keys"

  
  name.g <- as.character(match.call())[2]
  name.x <- as.character(match.call())[3]
  
  # Checking the values of the arguments
  
  # Are x and g data frames ?
  if (!is.data.frame(x))
    stop(paste(name.x, "is not a data frame."))
  if (!is.data.frame(g))
    stop(paste(name.g, "is not a data frame."))
  
  # Is keyx the name of a column of x data frame?
  if (! keyx %in% colnames(x))
    stop(paste("There is no", keyx, "variable in", name.x))
  
  # If keyg is not "rownames", is it the name of a column of g data frame?
  if (keyg != "rownames") {
    if (! keyg %in% colnames(g))
    stop(paste("There is no", keyg, "variable in", name.g))
  }
  
  if (keyg == "rownames") {
    # Add a column as first column of g (key)
    g <- data.frame(key = rownames(g), g)
    keyg <- "key"
  }
  
  # Checking the number of columns of g and x
  if (ncol(g) <= 1)
    stop(paste(name.g, "must be a data frame with at least one column (at least two if keyg != 'rownames')."))
  if (ncol(x) <= 2)
    stop(paste(name.x, "must be a data frame with at least two columns."))
  
  # Check if each group in keyg occurs only once
  if (max(table(g[, keyg])) > 1)
    stop(paste("A level of", name.g, "[,", keyg, "] cannot occur more than once."))
    
  # If nag.rm = TRUE: suppression of the lines for which g[, keyg] is NA
  if (nag.rm) {
    g.ret <- g[!is.na(g[, keyg]), ]
  } else {
    g.ret <- g
  }
  
  # If nax.rm = TRUE: suppression of the lines for which x[, keyx] is NA
  if (nax.rm) {
    x.ret <- x[!is.na(x[, keyx]), ]
  } else {
    x.ret <- x
  }
  
  # Creation of the folder
  foldh <- list(df1 = g.ret, df2 = x.ret)
  
  names(foldh) <- c(name.g,name.x)
  class(foldh) <- "folderh"
  attr(foldh, "keys") <- c(key1 = keyg, key2 = keyx)
  
  return(foldh)
}
