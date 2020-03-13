as.foldert.array <- function(x, ind = 1, var = 2, time = 3, ...)
{
  # x:         an array with 3 dimensions: (individuals x variables x times).
  # ind:       1, 2 or 3. Index of the observations.
  # var:       1, 2 or 3. Index of the variables
  # time:      1, 2 or 3. Index of the times
  
  name.x <- deparse(substitute(x))
  name.g <- deparse(substitute(timecol))
  
  # Checking of the arguments
  if (!is.array(x))
    stop(paste(name.x, "is not an array."))
  if (length(dim(x)) != 3)
    stop(paste0(name.x, ": wrong number of dimensions."))
  
  # If ind, var and time are not, respectively, the 1st, 2nd and 3rd dimensions of x,
  # permute the dimensions
  if (ind!=1 | var!=2 | time!=3) {
    x <- aperm(x, c(ind, var, time))
  }
  
  # Vector of times
  ntimes <- dim(x)[3]
  times <- as.ordered(dimnames(x)[[3]])
  if (is.null(times))
    times <- as.ordered(as.character(1:ntimes))
  
  # Building of the list of data frames
  fold <- list()
  for (k in 1:ntimes) {
    xk <- x[,, k, drop = FALSE]
    fold[[k]] <- as.data.frame(xk, stringsAsFactors = TRUE)
    colnames(fold[[k]]) <- colnames(xk)
  }
  names(fold) <- times
  
  # Creation of the foldert
  foldt <- foldert(fold, times = times, rows.select = "union")
  
  return(foldt)
}