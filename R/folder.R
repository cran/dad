# Prendra en argument :
#   - soit 2 (ou +) data frames
#   - soit liste de data frames
#
# On écrira une fonction df2folder() qui prendra en argument un data.frame
# et le nom de la variable de grouupe.

folder <-
function(x1, x2 = NULL, ..., cols.select = "intersect", rows.select = "") 
{
  # x1  :         data.frame or list of data frames; the data.
  #               - If x1 is a data.frame, it contains the first data set of the
  #                 folder which will be built, and x2 must be provided.
  #               - If x1 is a list of data frames, x2 must not be provided.
  # x2  :         data.frame; the second data set.
  # ... :         other data.frames (optional)
  # cols.select : string. It can be:
  #               - "intersect": only columns (column names) that are common
  #                              to every data frames are kept.
  #                              We will have: "attr(fold, same.cols) = TRUE".
  #               - "union":     all the columns (and column names) of every
  #                              data frames are kept; the lines are completed
  #                              by NA if necessary.
  #                              We will have: "attr(fold, same.cols) = TRUE".
  #               - A character vector, its elements being column names of the data frames.
  #                 If it is so, the columns with names given by rows.select are the selected
  #                 and are the columns of the data frame elements of the returned "foldert".
  #                 Therefore we have: same.cols = TRUE.
  # rows.select:  character. It can be:
  #                 - "union" or "intersect": the rownames of the data frames
  #                   in the returned foldert are the union or the intersection
  #                   of all rownames of the data frame arguments.
  #                   Therefore we have: same.rows = TRUE.
  #                 - "": the rownames are not considered to be the same.
  #                   A unique name is computed for each individual.
  #                   Therefore: same.rows = FALSE.
  #               Dans tous les cas, les data frames du foldert auront tous les mêmes noms de colonnes.
  #               Donc l'attribut "same.cols" est supprimé :
  #               il n'a plus de raison d'être car il serait toujours TRUE.
  
  # Checking the class or value of each argument

  if (!is.list(x1))
    stop("x1 must be a data frame or a list of data frames.")
  
  if ((!is.data.frame(x1)) & (!is.null(x2)))
    warning("If x1 is not a data frame, x2 is omitted.")
  
  if ((is.data.frame(x1)) & (is.null(x2)))
    stop("You cannot build a 'folder' with only one data frame.\nIf x1 is a data frame, x2 must be provided.")
  
  if ((!is.data.frame(x1)) & (length(x1) == 1))
    stop("You cannot build a 'folder' with only one data frame.\nIf x1 is a list, it must contain at least two data frames.")
  
  if ((is.data.frame(x1)) & (!is.data.frame(x2)))
    stop("If x1 is a data frame, x2 must also be a data frame.")
  
  # if (!cols.select %in% c("intersect", "union"))
  #   stop("cols.select: wrong value. It must be either 'intersect' or 'union'")
  
  if (!is.data.frame(x1)) {
    # x1 is a list of data frames
    class.arg <- "l"
  } else {
    # x1 and x2 are data frames
    if (is.data.frame(x2)) {
      class.arg <- "d2"
    }
    dots <- list(...)
    if (length(dots) > 0) {
      if (all(unlist(lapply(dots, is.data.frame)))) {
        # All arguments in "..." are data frames
        class.arg <- "d3"
      } else {
        warning(paste("Argument(s)", names(dots)[unlist(lapply(dots, is.data.frame))], "is/are no data frame(s).\n", names(dots), "arguments will not be used."))
      }
    }
  }
  
  if (! rows.select %in% c("union", "intersect", ""))
    rows.select <- ""
  
  switch(class.arg,
    d2 = {
      # Creation of the list
      fold <- list(x1, x2)
      
      # Number of datasets in the folder
      ndata <- 2
  
      # Names of the elements of the folder
      namesfold <- c(deparse(substitute(x1)), deparse(substitute(x2)))
      names(fold) <- namesfold
    },
    d3 = {
      # Creation of the list
      fold <- list(x1, x2)
      if (!all(unlist(lapply(dots, is.data.frame))))
        stop("All arguments in '...' must be data frames")
      fold <- c(fold, dots)
      
      # Number of data frames in the folder
      ndata <- length(fold)
      
      # Names of the elements of the folder
      namesfold <- c(deparse(substitute(x1)), deparse(substitute(x2)))
      namesfold <- c(namesfold, deparse(substitute(...)))
      names(fold) <- namesfold
    },
    l = {
      # Check if all elements of x1 are data frames
      if (!all(unlist(lapply(x1, is.data.frame))))
        stop("All elements of x1 must be data frames.")
      
      # Creation of the list
      fold <- x1
      
      # Number of datasets in the folder
      ndata <- length(fold)
    }
  )
  
  # If cols.select is a character vector:
  # Select the corresponding columns in each elements of fold
  if (! cols.select[1] %in% c("union", "intersect")) {
    for (n in 1:ndata) {
      fold[[n]] <- fold[[n]][intersect(cols.select, colnames(fold[[n]]))]
    }
    # Then, the columns will be selected, so that the elements of fold all have the same column names,
    # corresponding to the 'cols.select' argument (completing with NA columns if necessary)
    cols.select <- "union"
  }
  
  # If (cols.select == "intersect"): only the columns whose names are common
  # to each element of 'fold' will be kept.
  if (cols.select == "intersect") {
    cnames.l <- lapply(fold, names)
    cnames <- cnames.l[[1]]
    for (n in 2:ndata)
      cnames <- intersect(cnames, cnames.l[[n]])
    for (n in 1:ndata) {
      fold[[n]] <- fold[[n]][cnames]
    }
    # same.cols <- TRUE
  }
  
  # If (cols.select == "union"): add columns to each element of 'fold',
  # so that they all have the same columns and colnames.
  if (cols.select == "union") {
    cnames <- unique(unlist(lapply(fold, names)))
    adjcnames <- as.data.frame(matrix(nrow = 0, ncol = length(cnames)))
    #as.data.frame(t(vector(length = length(cnames))))
    colnames(adjcnames) <- cnames
    for (n in 1:ndata) {
      if (ncol(fold[[n]]) > 0) {
        foldn <- merge(data.frame(".rownames" = rownames(fold[[n]]), fold[[n]]), adjcnames, all = TRUE, sort = FALSE)[1:nrow(fold[[n]]), , drop = FALSE]
        rownames(foldn) <- foldn$".rownames"
        fold[[n]] <- foldn[cnames]
      } else {
        foldn <- matrix(NA, nrow = nrow(fold[[n]]), ncol = ncol(adjcnames), dimnames = list(rownames(fold[[n]]), cnames))
        fold[[n]] <- as.data.frame(foldn)
      }
    }
    # same.cols <- TRUE
  }
  
  # If (rows.select == "intersect"): only the rows whose names are common
  # to each element of 'fold' will be kept.
  if (rows.select == "intersect"){
    rnames.l <- lapply(fold, rownames)
    rnames <- rnames.l[[1]]
    for (n in 2:ndata)
      rnames <- intersect(rnames, rnames.l[[n]])
    rnames <- sort(rnames)
    for (n in 1:ndata) {
      fold[[n]] <- fold[[n]][rnames, , drop = FALSE]
    }
    same.rows <- TRUE
  }
  # If (rows.select == "union"): add columns to each element of 'fold',
  # so that they all have the same rows and rownames.
  if (rows.select == "union") {
    rnames <- sort(unique(unlist(lapply(fold, rownames))))
    for (n in 1:ndata) {
      foldn <- fold[[n]]
      adjrnames <- rnames[! rnames %in% rownames(foldn)]
      if (length(adjrnames) > 0) {
        adjrows <- as.data.frame(matrix(NA, nrow = length(adjrnames), ncol = ncol(foldn), dimnames = list(adjrnames, colnames(foldn))))
        foldn <- rbind(foldn, adjrows)
      }
      fold[[n]] <- foldn[rnames, , drop = FALSE]
    }
    same.rows <- TRUE
  }
  if (rows.select == "") {
    # For each element of fold, all the lines are kept, and they are made unique by adding the name of this element
    for (n in 1:ndata) {
      if (nrow(fold[[n]]) > 0)
        rownames(fold[[n]]) <- paste(names(fold)[n], rownames(fold[[n]]), sep = ".")
    }
    same.rows <- FALSE
  }
  
  # Object which will be returned
  class(fold) <- "folder"
  # attr(fold, "same.cols") <- same.cols
  attr(fold, "same.rows") <- same.rows# FALSE
  
  return(fold)
}
