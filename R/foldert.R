foldert <- function(x1, x2 = NULL, ..., times = NULL, cols.select = "intersect", rows.select = "")
  #, same.rows = (rows.select %in% c("union", "intersect"))  
{
  # x1:          list of data frames or data frame
  # x2:          data frame; defaults to NULL (not considered if x1 is a list of data frame)
  # ...:         if x1 is a data frame, one or more data frames.
  #              Omitted if x1 is a list of data frame.
  # cols.select: character vector. It can be:
  #                - "union": the colnames of the data frames in the returned foldert
  #                  are the union of all colnames of the data frame arguments.
  #                  Therefore we have: same.cols = TRUE.
  #                - "intersect": the colnames of the data frames in the returned foldert
  #                  are the intersect of the colnames of the data frame arguments.
  #                  Therefore we have: same.cols = TRUE.
  #                - A character vector, its elements being column names of the data frames.
  #                  If it is so, the columns with names given by rows.select are the selected
  #                  and are the columns of the data frame elements of the returned "foldert".
  #                  Therefore we have: same.cols = TRUE.
  # rows.select: character. It can be:
  #                - "union" or "intersect": the rownames of the data frames
  #                  in the returned foldert are the union or the intersection
  #                  of all rownames of the data frame arguments.
  #                  Therefore we have: same.rows = TRUE.
  #                - "": the rownames are not considered to be the same.
  #                  A unique name is computed for each individual.
  #                  Therefore: same.rows = FALSE.
  #              Dans tous les cas, les data frames du foldert auront tous les mêmes noms de colonnes.
  #              Donc l'attribut "same.cols" est supprimé :
  #              il n'a plus de raison d'être car il serait toujours TRUE.
  #
  # Arguments which will be added later: data.times, data.rows, data.cols (data frames).
  
  # Checking the class or value of each argument
  
  if (!is.list(x1))
    stop("x1 must be a data frame or a list of data frames.")
  
  if ((!is.data.frame(x1)) & (!is.null(x2)))
    warning("If x1 is not a data frame, x2 is omitted.")
  
  if ((is.data.frame(x1)) & (is.null(x2)))
    stop("You cannot build a 'foldert' with only one data frame.\nIf x1 is a data frame, x2 must be provided.")
  
  if ((!is.data.frame(x1)) & (length(x1) == 1))
    stop("You cannot build a 'foldert' with only one data frame.\nIf x1 is a list, it must contain at least two data frames.")
  
  if ((is.data.frame(x1)) & (!is.data.frame(x2)))
    stop("If x1 is a data frame, x2 must also be a data frame.")
  
  if (!is.data.frame(x1)) {
    # x1 is a list of data frames
    nb.data.frames <- length(x1)
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
        nb.data.frames <- length(dots) + 2 
      } else {
        warning(paste("Argument(s)", names(dots)[unlist(lapply(dots, is.data.frame))], "is/are no data frame(s).\n", names(dots), "arguments will not be used."))
      }
    }
  }
  
  if ((is.null(times)) & (!is.data.frame(x1))) {
    # If times was omitted and x1 is a list of data frames:
    # the times are the names of its elements.
    times <- names(x1)
    if (!any(is.na(strptime(times, format = "%Y-%m-%d"))))
      {
        times <- as.Date(times)
      } else if (!any(is.na(as.numeric(times))))
      {
        times <- as.numeric(times)
      } else {
        stop("times argument missing,\nand the names of the elements of the list x1 cannot be converted into numeric or Date.")
      }
  }
  
  if ((is.null(times)) & (is.data.frame(x1)))  {
    # If times was omitted and x1 is a data frame:
    times = 1:nb.data.frames
  }
  
  if ((is.null(times)) & (!is.data.frame(x1)) & (is.null(names(x1))))  {
    # If times was omitted and x1 is a list of data frames, the names of its element being NULL:
    times = 1:nb.data.frames
  }
      
  if (! ((is.ordered(times)) | (is.numeric(times)) | 
         ("Date" %in% class(times)) | 
      ("POSIXlt" %in% class(times)) | ("POSIXct" %in% class(times))))
    stop("times must be either an ordered factor or a numeric vector or a Date, POSIXlt or POSIXct object.")
  
  if (! rows.select %in% c("union", "intersect", ""))
    rows.select <- ""
  
  # if ((same.rows) & (rows.select == "")) {
  #   warning("If rows.select is not 'union' nor 'intersect', same.rows cannot be TRUE")
  #   same.rows <- FALSE
  # }
  
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
           if (!prod(unlist(lapply(dots, is.data.frame))))
             stop("All arguments in '...' must be data frames")
           fold <- c(fold, dots)
           
           # Number of data frames in the folder
           ndata <- length(fold)
           
           # Names of the elements of the folder
           namesfold <- c(deparse(substitute(x1)), deparse(substitute(x2)))
           # To get the names of the elements in 'three dots', it is slightly different (and less simple)
           substdots <- substitute(list(...))[-1]
           namesdots <- sapply(substdots, deparse)
           namesfold <- c(namesfold, namesdots)
           names(fold) <- namesfold
         },
         l = {
           # Check if all elements of x1 are data frames
           if (any(! unlist(lapply(x1, is.data.frame))))
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
  
#   # If (cols.select == ""): check if all elements of 'fold' have the same column names.
#   # If they have, same.cols = TRUE. Otherwise same.cols = FALSE.
#   if (cols.select == "") {
#     sameCols <- sapply(lapply(fold[2:ndata], colnames), identical, colnames(fold[[1]]))
# #    for (n in 2:ndata) {
# #      if (!identical(colnames(fold[[1]]), colnames(fold[[n]])))
# #        same.cols <- FALSE
# #    }
#     same.cols <- all(sameCols)
#   }
  
  # if (same.rows) { # If same.rows is TRUE, select the rows of the data frames
  
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
  
  # times: dates of observations.
  names(fold) <- times
  attr(fold, "times") <- times
  # attr(fold, "same.cols") <- same.cols
  attr(fold, "same.rows") <- same.rows
  
  class(fold)  <- "foldert"
  
  return(fold)
}
