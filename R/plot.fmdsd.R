plot.fmdsd <-
  function(x, nscore=1:3, main="MDS of probability density functions", sub.title=NULL, color = NULL, fontsize.points = 1.5, ...)
  {
    inertia=x$inertia$inertia
    coor=x$scores[, -1]
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
      graph <- ggplot(coor)
      graph <- graph + aes_q(as.name(names(coor)[i1]), as.name(names(coor)[i2]),
                             label = as.character(group))
      if (!is.null(color)) {
        namecol <- deparse(substitute(color))
        assign(namecol, color)
        graph <- graph + aes_q(color = as.name(namecol))
      }
      graph <- graph + geom_text(aes(fontface = "bold"), size = 4.2*fontsize.points)
      graph <- graph + labs(title = main, subtitle = sub.title,
                            x = paste0(names(coor)[i1], " (", inertia[i1], "%)"),
                            y = paste0(names(coor)[i2], " (", inertia[i2], "%)"))
      print(graph)
    }
    return(invisible(NULL))
  }
