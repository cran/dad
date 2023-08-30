\name{print.fdiscd.misclass}
\alias{print.fdiscd.misclass}
%\alias{print}
\title{
    Printing results of discriminant analysis of probability density functions
}
\description{
    Applies to an object of class \code{"fdiscd.misclass"}. Prints the numerical results of \code{\link{fdiscd.misclass}}.
}
\usage{
\method{print}{fdiscd.misclass}(x, dist.print=FALSE, prox.print=FALSE, digits=2, ...)
}
\arguments{
  \item{x}{
      object of class \code{"fdiscd.misclass"}, returned by \code{\link{fdiscd.misclass}}.
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
    Boumaza, R. (2004). Discriminant analysis with independently repeated multivariate measurements: an \eqn{L^2} approach. Computational Statistics & Data Analysis, 47, 823-843.
    
    Rudrauf, J.M., Boumaza, R. (2001). Contribution à l'étude de l'architecture médiévale: les caractéristiques des pierres à bossage des châteaux forts alsaciens, Centre de Recherches Archéologiques Médiévales de Saverne, 5, 5-38.
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
    \link{fdiscd.misclass}; \link{print}.
}
\examples{
data(castles.dated)
castlesfh <- folderh(castles.dated$periods, "castle", castles.dated$stones)
result <- fdiscd.misclass(castlesfh, "period")
print(result)
print(result, dist.print=TRUE)
print(result, prox.print=TRUE)
}
