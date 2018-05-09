fpcat <-
function(xf, gaussiand=TRUE, kern = NULL, windowh=NULL,
			normed=TRUE, centered=FALSE, data.centered=FALSE, data.scaled=FALSE, 
      common.variance=FALSE, nb.factors=3, nb.values=10, sub.title="",
			plot.eigen=TRUE, plot.score=FALSE, nscore=1:3, group.name="time", filename=NULL)
{
if (!is.foldert(xf)){
  stop("fpcat applies to an object of class 'foldert'.")
}

times <- attr(xf, "times")

# Perform the FPCA, using fpcad()
class(xf) <- "folder"
results <- fpcad(xf, gaussiand=gaussiand, kern = kern, windowh=windowh,
                 normed=normed, centered=centered, data.centered=data.centered, data.scaled=data.scaled, 
                 common.variance=common.variance, nb.factors=nb.factors, nb.values=nb.values, sub.title=sub.title,
                 plot.eigen=plot.eigen, plot.score=plot.score, nscore=nscore, group.name=group.name, filename=filename)

# Object of class "fpcat"
results$call <- match.call()
results <- append(results, list(times), after = 3) # (Add the times vector as the 4th element of the 'foldert')
names(results)[4] <- "times"
class(results) <- "fpcat"

# Returning the list of results
return(results)
}
