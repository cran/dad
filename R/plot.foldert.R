plot.foldert <- function(x, which, na.inter = TRUE, type = "l", lty = 1:5, col = 1:6, ylim = NULL, ylab = which, main = "", ...)
{
  # x:        an object of class "foldert".
  # which:    name of the variable which will be represented.
  # na.inter: logical.
  #           - If TRUE (default), the NA values are interpolated.
  #           - If FALSE, matplot is used and there is an interruption
  #             of the curve for each missing value.
  # ...:      other arguments of matplot().
  
  # Vector of the times
  times <- attr(x, "times")
  
  # Matrix of the observations of the variable given by which among time:
  # - lines: the individuals
  # - columns: the observation times
  mdata <- matrix(nrow = nrow(x[[1]]), ncol = length(x),
                  dimnames = list(rownames(x[[1]]), names(x)))
  for (tt in names(x)) {
    for (ind in rownames(mdata))
      mdata[ind, tt] <- x[[tt]][ind, which]
  }
  
  # Suppression of NA columns
  colna <- apply(apply(mdata, 2, is.na), 2, all)
  if (any(colna))
  {
    mdata <- mdata[, -which(colna), drop = FALSE]
    # and suppression of the corresponding times
    times <- times[-which(colna)]
  }
  
  if ("Date" %in% class(times))
    times <- as.POSIXct(times)

  # The matplot (if na.inter, the plot is not displayed)
  if (is.null(ylim))
  {
    ylim = range(mdata, na.rm = TRUE)*c(0, 1.2)
  }
  # fdat <- fdata(mdata, times)
  # plot(fdat, type = paste0(type[!na.inter], "n"[na.inter]),
  #      lty = lty, col = col, ylim = ylim, xlab = "", ylab = ylab, main = "", ...)
  matplot(times, t(mdata), type = paste0(type[!na.inter],
          "n"[na.inter]), lty = lty, col = col, ylim = ylim, xlab = "", ylab = ylab, ...)
  # if ("Date" %in% class(times))
  # {
  #   xax <- seq(min(times), max(times), length = min(length(times), 5))
  #   axis(1, xax, xax, las = 2)
  # } else
  # {
  #   axis(1, numeric, times, las = 2)
  # }
  legend("topright", legend = rownames(mdata), lty = lty, col = col, ncol = 3)
  

  # If na.inter is TRUE: add each variable to the graph; the NA are suppressed
  if (na.inter)
  {
    # If the length of lty and col is less than the number of curves to be plotted,
    # they are duplicated
    nind <- nrow(mdata)
    if (length(lty) < nind)
      lty <- rep_len(lty, nind)
    if (length(col) < nind)
      col <- rep_len(col, nind)
    
    for (k in 1:nind)
    {
      yind <- as.numeric(mdata[k, ])
      notna <- which(!is.na(yind))
      yind <- yind[notna]
      xind <- times[notna]
      points(xind, yind, type = type, col = col[k], lty = lty[k])
    }
  }
  return(invisible())
}