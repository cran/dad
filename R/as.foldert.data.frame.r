as.foldert.data.frame <- function(x, method = 1, ind = 1, timecol = 2, nvar = NULL, same.rows = TRUE, ...)
{
  # x:         a data frame.
  # method:    1 or 2.
  #            - If method=1, there is a column containing the identifiers of the individuals and a column containing the times.
  #            - If method=2, there is a column containing the identifiers of the individuals, and the observations are organized as follows:
  #                 * the observations corresponding to the 1st time are on columns timecol:(timecol+nvar-1)
  #                 * the observations corresponding to the 2nd time are on columns (timecol+nvar):(timecol+2*nvar-1)
  #                 * and so on.
  # ind:       name or number of the column of the identifier of the individuals.
  # timecol:   indicates the column(s) corresponding to the times of observation.
  #            - If (method==1), it is the name (or number) of the column containing the times.
  #            - If (method==2), it gives the name or number of the first column corresponding to the first observation.
  #              In this case, nvar must be provided.
  # nvar:      number of variables measured at each time. Omitted if (method==1). Must be provided when (method==2).
  # same.rows: logical. Will the data frames have the same row names in the returned foldert? Default: TRUE.
  #            If (method==2), same.rows is necessarily TRUE.
  
  if (is.numeric(ind))
    ind <- colnames(x)[ind]
  if (is.numeric(timecol))
    timecol <- colnames(x)[timecol]

  method <- paste0("method", method)
  switch(method,
         method1 = {
           name.x <- deparse(substitute(x))
           name.g <- deparse(substitute(timecol))
           
           # Checking of the arguments
           if (!is.data.frame(x))
             stop(paste(name.x, "is not a data frame."))
           if (!timecol %in% colnames(x))
             stop(paste(name.g, " is not a column name of ", name.x, ".", sep = ""))
           
           # The individuals
           jind <- which(colnames(x) == ind)

           # The groups (times)
           jg <- which(colnames(x) == timecol)
           g <- x[, jg]
           if ((!is.numeric(g)) & (!is.ordered(g)) & (! "Date" %in% class(g))
               & (! "POSIXlt" %in% class(g)) & (! "POSIXct" %in% class(g))) {
             stop(paste(name.x, "[, ", name.g, "]", " must be of class 'numeric', 'ordered' or 'Date'.", sep = ""))
           }
           g <- as.ordered(g)
           glev <- levels(g)
           
           # Building of the list of data frames
           fold <- list()
           for (l in glev) {
             fold.l <- x[g == l, ]
             rownames(fold.l) <- fold.l[, jind]
             fold <- c(fold, list(fold.l[-c(jind, jg)]))
           }
           names(fold) <- glev
           
           # Creation of the foldert
           foldt <- foldert(fold, times = sort(unique(x[, jg])), cols.select = "union",
                            rows.select = paste0(""[!same.rows], "union"[same.rows]))
         },
         method2 = {
           # If method=2, same.rows must be TRUE
           if (!same.rows) {
             same.rows <- TRUE
             warning("When method==2, the returned 'foldert' always has the same row names.\nTherefore 'same.rows' cannnot be FALSE; it was set to TRUE.")
           }
           
           # column of the individual identifiers
           jind <- which(colnames(x) == ind)
           
           # Column of the 1st variable at time 1:
           jt <- which(colnames(x) == timecol) - 1
           # If several columns have this name, the first of these columns is considered
           if (length(jt) > 1)
           {
             warning("Duplicated column names: Several columns of x are named ", timecol, "\nThe first of these column is considered to contain the observations at the first time.")
             jt <- jt[1]
           }
           
#            browser()
           # Building the list of data frames
           cnames <- c("ind", colnames(x)[jt + 1:nvar])
           fold <- list()
           i <- 0
           while((jt+nvar) <= ncol(x)) {
             # cat("\n")
             # print(colnames(x)[c(jind, jt + 1:nvar)])
             xj <- x[, c(jind, jt + 1:nvar)]
             colnames(xj) <- cnames
             fold <- c(fold, list(xj))
             jt <- jt + nvar
           }
           if (sum(sapply(fold, ncol)) < (ncol(x)-jt))
             warning(paste0("The number of columns expected to contain the observations are not a multiple of nvar.\n",
                            "The last columns of x data frames are omitted in the resulting 'foldert' object."))
           
           # The times (ordered factor)
           times <- paste0("t", 1:length(fold))
           
           # Creation of the foldert
           foldt <- foldert(fold, times = ordered(times, levels = times),
                            cols.select = "union", rows.select = "union")
         }
  )
  
  return(foldt)
}