\name{print.fhclustd}
\alias{print.fhclustd}
%\alias{print}
\title{
    Printing results of a hierarchical clustering of probability density functions
}
\description{
    \code{print} function, applied to an object of class \code{"fhclustd"}, prints numerical results of \link{fhclustd} .
}
\usage{
\method{print}{fhclustd}(x, dist.print=FALSE, digits=2, ...)
}
\arguments{
  \item{x}{
      object of class \code{"fhclustd"}, returned by \link{fhclustd}.
}
  \item{dist.print}{
      logical. If \code{TRUE} (default: \code{FALSE}), prints the matrix of distances between the groups (densities).
}
  \item{digits}{
      numerical. Number of significant digits for the display of numerical results.
}
  \item{...}{
      optional arguments to \code{print} methods.
}
}
\details{
    If \code{dist.print = TRUE}, the distances between groups are displayed.
    
    By default, the result of the clustering is printed. The display is the same as that of the \code{\link{print.hclust}} function.
}
%\references{
%
%}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
    \link{fhclustd}; \link{print}.
}
\examples{
data(castles.dated)
xf <- as.folder(castles.dated$stones)
\dontrun{
result <- fhclustd(xf)
print(result)
print(result, dist.print = TRUE)
}
}
