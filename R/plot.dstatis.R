plot.dstatis <-
  function(x, nscore=c(1, 2), sub.title=NULL, color = NULL, fontsize.points = 1.5, ...)
  {
    inertia=x$inertia$inertia
    coor=x$scores[, -1]
    if (length(nscore) > 2)
      nscore <- nscore[1:2]
    if (max(nscore)>ncol(coor))
      stop("The components of nscore must be smaller than the number of score columns in the x$scores data frame")
    group=x$scores[, 1]
    ind=nscore
    i1=nscore[1]; i2=nscore[2]
    graph <- ggplot(coor) + aes_q(as.name(names(coor)[i1]), as.name(names(coor)[i2]),
                                  label = as.character(group))
    if (!is.null(color)) {
      namecol <- deparse(substitute(color))
      assign(namecol, color)
      graph <- graph + aes_q(color = as.name(namecol))
    }
    graph <- graph + geom_text(aes(fontface = "bold"), size = 4.2*fontsize.points)
    graph <- graph + labs(title = "Dual Statis", subtitle = sub.title,
                          x = paste(names(coor)[i1], " (", inertia[i1], "%)"),
                          y = paste(names(coor)[i2], " (", inertia[i2], "%)"))
    print(graph)
    return(invisible(NULL))
  }
