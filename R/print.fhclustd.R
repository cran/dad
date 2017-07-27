print.fhclustd <-
function(x, dist.print=FALSE, digits=2, ...)
{
# Display of the distances between groups (optional)
if (dist.print) 
  {cat("distances between group\n") 
   print(x$distances, digits=digits, ...)
  }
print(x$clust)
return(invisible(x))
}
