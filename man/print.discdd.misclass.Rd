\name{print.discdd.misclass}
\alias{print.discdd.misclass}
%\alias{print}
\title{
    Printing results of discriminant analysis of discrete probability distributions
}
\description{
    Applies to an object of class \code{"discdd.misclass"}. Prints the numerical results of \code{\link{discdd.misclass}}.
}
\usage{
\method{print}{discdd.misclass}(x, dist.print=FALSE, prox.print=FALSE, digits=2, ...)
}
\arguments{
  \item{x}{
      object of class \code{"discdd.misclass"}, returned by \code{\link{discdd.misclass}}.
}
  \item{dist.print}{
      logical. Its default value is \code{FALSE}. If \code{TRUE}, prints the matrix of distances between, on one side, the groups (densities) and, on the other side, the classes (of groups or densities).
}
  \item{prox.print}{
      logical. Its default value is \code{FALSE}. If \code{TRUE}, prints the matrix of proximity indices (in percent) )between, on one side, the groups (densities) and, on the other side, the classes (of groups or densities).
}
  \item{digits}{
      numeric. Number of significant digits for the display of numerical results.
}
  \item{...}{
      optional arguments to \code{print} methods.
}
}
\details{
    By default, are printed the whole misallocation ratio, the confusion matrix (allocations versus origins) with the misallocation ratios per class, and the data frame whose rows are the groups, and whose columns are the origin classes and allocation classes, and a logical variable indicating misclassification. 
    
    If \code{dist.print = TRUE} or \code{prox.print = TRUE}, the distances or proximity indices (in percent) between groups and classes, are displayed.
}
\references{
    Rudrauf, J.M., Boumaza, R. (2001). Contribution ? l'étude de l'architecture médiévale: les caractéristiques des pierres à bossage des châteaux forts alsaciens, Centre de Recherches Archéologiques médiévales de Saverne, 5, 5-38.
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
    \link{discdd.misclass}; \link{print}.
}
\examples{
data("castles.dated")
stones <- castles.dated$stones
periods <- castles.dated$periods
stones$height <- cut(stones$height, breaks = c(19, 27, 40, 71), include.lowest = TRUE)
stones$width <- cut(stones$width, breaks = c(24, 45, 62, 144), include.lowest = TRUE)
stones$edging <- cut(stones$edging, breaks = c(0, 3, 4, 8), include.lowest = TRUE)
stones$boss <- cut(stones$boss, breaks = c(0, 6, 9, 20), include.lowest = TRUE )

castlefh <- folderh(periods, "castle", stones)

res <- discdd.misclass(castlefh, "period")

print(res)
}
