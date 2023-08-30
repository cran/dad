\name{print.fdiscd.predict}
\alias{print.fdiscd.predict}
%\alias{print}
\title{
    Printing results of discriminant analysis of probability density functions
}
\description{
    \code{print} function, applied to an object of class \code{"fdiscd.predict"}, prints numerical results of \link{fdiscd.predict} .
}
\usage{
\method{print}{fdiscd.predict}(x, dist.print=TRUE, prox.print=FALSE, digits=2, ...)
}
\arguments{
  \item{x}{
      object of class \code{"fdiscd.predict"}, returned by \link{fdiscd.predict}.
}
  \item{dist.print}{
      logical. If \code{TRUE} (the default), prints the matrix of distances between, on one side, the groups (densities) and, on the other side, the classes (of groups or densities).
}
  \item{prox.print}{
      logical. Its default value is \code{FALSE}. If \code{TRUE}, prints the matrix of proximity indices between, on one side, the groups (densities) and, on the other side, the classes (of groups or densities).
}
  \item{digits}{
      numerical. Number of significant digits for the display of numerical results.
}
  \item{...}{
      optional arguments to \code{print} methods.
}
}
\details{
    By default, are printed:
    \itemize{
    \item if available (if \code{misclass.ratio} argument of \code{\link{fdiscd.predict}} was \code{TRUE}), the whole misallocation ratio, the confusion matrix (allocations versus origins) and the misallocation ratio per class are printed.
    \item the data frame the rows of which are the groups, and the columns of which are of the origin (\code{NA} if not available) and allocation classes. 
    }
    
    If \code{dist.print = TRUE} or \code{prox.print = TRUE}, the distances or proximity indices between groups and classes, are displayed.
}
\references{
    Boumaza, R. (2004). Discriminant analysis with independently repeated multivariate measurements: an \eqn{L^2} approach. Computational Statistics & Data Analysis, 47, 823-843.
    
    Rudrauf, J.M., Boumaza, R. (2001). Contribution à l'étude de l'architecture médiévale: les caractéristiques des pierres à bossage des châteaux forts alsaciens, Centre de Recherches Archéologiques médiévales de Saverne, 5, 5-38.
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
    \link{fdiscd.predict}; \link{print}.
}
\examples{
data(castles.dated)
data(castles.nondated)
castles.stones <- rbind(castles.dated$stones, castles.nondated$stones)
castles.periods <- rbind(castles.dated$periods, castles.nondated$periods)
castlesfh <- folderh(castles.periods, "castle", castles.stones)
result <- fdiscd.predict(castlesfh, "period")
print(result)
print(result, prox.print=TRUE)
}
