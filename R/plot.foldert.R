#plot.foldert <- function(x, which, na.inter = TRUE, type = "l", lty = 1:5, col = 1:6, ylim = NULL, ylab = which, main = "", ...)
plot.foldert <- function(x, which, na.inter = TRUE, type = "l", ylim = NULL, ylab = which, main = "", ...)
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
  
  # In each data.frame, suppress rows that are entirely NA
  x <- lapply(x, function(tab) {
    isna <- apply(tab, 1, function(x) all(is.na(x)))
    tab <- tab[!isna, ]
    return(tab)
  })

  # Add row names as a supplementary column in each data.frame of x
  x <- lapply(x, function(tab) {
    data.frame(ind = rownames(tab), tab, stringsAsFactors = TRUE)
  })
  
  data_ <- as.data.frame(foldert(x, times = times))
  # names(data_) <- sub(which, "value", names(data_))
  
  # If na.inter is TRUE: the NA are suppressed
  if (na.inter) {
    data_ <- data_[!is.na(data_[, which]), ]
  }
  
  # nind <- nlevels(data_$ind)
  # lty <- rep_len(lty, nind)
  # col <- rep_len(col, nind)
  
  graph <- ggplot(data_)
  graph <- graph + aes_q(as.name("time"), as.name(which),
                         group = as.name("ind"),
                         colour = as.name("ind"), linetype = as.name("ind"))
  graph <- graph + geom_line()
  graph <- graph + labs(title = main, y = ylab)
  if (!is.null(ylim)) graph <- graph + ylim(ylim[1], ylim[2])
  print(graph)
  
  return(invisible())
}

