plot.fpcat <-
function(x, nscore=1:3, main="PCA of probability density functions", sub.title=NULL, ...)
{
  times <- x$times
  numtimes <- as.numeric(times)
  
  inertia=x$inertia$inertia
  coor=x$scores[, -1]
  ngroup <- nrow(coor)

  if (max(nscore)>ncol(coor))
    stop("The components of nscore must be smaller than the number of score columns in the x$scores data frame")
  group=x$scores[, 1]
  ind=combn(nscore, 2)
  for (j in 1:ncol(ind)) 
  {
    i1=ind[1, j]; i2=ind[2, j]
    if (.Device %in% c("null device", "X11", "windows", "quartz", "RStudioGD"))
      {
      dev.new()
      }
    coorij <- data.frame(coor[-nrow(coor), c(i1, i2)],
                         coor[-1, c(i1, i2)], stringsAsFactors = TRUE)
    names(coorij) <- c("x1", "y1", "x2", "y2")
    
    graph <- ggplot(coorij)
    graph <- graph + aes_q(x = as.name("x1"), y = as.name("y1"),
                           xend = as.name("x2"), yend = as.name("y2"))
    graph <- graph + geom_segment(arrow = arrow(unit(0.1, "inches")))
    graph <- graph + labs(title = main, subtitle = sub.title,
                          x = paste0(names(coor)[i1], " (", inertia[i1], "%)"),
                          y = paste0(names(coor)[i2], " (", inertia[i2], "%)"))
    print(graph)
  }
  
  if ("Date" %in% class(times))
    times <- as.POSIXct(times)
  
  for (i in nscore) {
    if (.Device %in% c("null device", "X11", "windows", "quartz", "RStudioGD")) {
      dev.new()
    }
    graph <- ggplot(data.frame(time = times, coor = coor[, i]), stringsAsFactors = TRUE)
    graph <- graph + aes_q(x = as.name("time"), y = as.name("coor"))
    graph <- graph + geom_line()
    graph <- graph + labs(title = main, subtitle = sub.title,
                          x = "time", y = paste0(names(coor)[i], " (", inertia[i], "%)"))
    print(graph)
  }
  
  return(invisible(NULL))
}
