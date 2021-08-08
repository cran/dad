plot.fmdsd <-
  function(x, nscore=c(1, 2), main="MDS of probability density functions", sub.title=NULL, color = NULL, fontsize.points = 1.5, ...)
  {
    if (length(nscore) > 2)
      warning(paste("Since dad-4, nscore must be a length 2 numeric vector. The scores number", nscore[1], "and", nscore[2], "are plotted."))
    nscore <- nscore[1:2]
    
    inertia=x$inertia$inertia
    coor=x$scores[-1]
    if (length(nscore) > 2)
      nscore <- nscore[1:2]
    if (max(nscore)>ncol(coor))
      stop("The components of nscore must be smaller than the number of score columns in the x$scores data frame")
    group=x$scores[, 1]
    i1=nscore[1]; i2=nscore[2]
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
    return(invisible(NULL))
  }
