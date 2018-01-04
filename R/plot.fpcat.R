plot.fpcat <-
function(x, nscore=1:3, sub.title=NULL, fontsize.points = 1.5, ...)
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
    par(ps=12);
    plot(coor[,i1],coor[,i2],type="n",
         main="Functional PCA of probability densities",sub=sub.title,
         xlab = paste("PC", i1, " (", inertia[i1], "%)"),
         ylab=paste("PC", i2, " (", inertia[i2], "%)"), ...);
#    par(ps=10);
    arrows(coor[-ngroup,i1], coor[-ngroup,i2], coor[-1,i1], coor[-1,i2], length = 0.1)
  }
  
  for (i in nscore) {
    if (.Device %in% c("null device", "X11", "windows", "quartz", "RStudioGD")) {
      dev.new()
    }
    par(ps=12);
    plot(numtimes,coor[,i],type="n",
         main="Functional PCA of probability densities",sub=sub.title,
         xlab = "time",ylab=paste("PC", i, " (", inertia[i], "%)"),
         xaxt = "n", ...);
    axis(1, numtimes, times)
#    par(ps=10);
#    text(times, coor[,i], as.character(group), cex=fontsize.points, font=2);
    arrows(numtimes[-ngroup], coor[-ngroup,i], numtimes[-1], coor[-1,i], length = 0.1)
  }
  
  return(invisible(NULL))
}
