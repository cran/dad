\name{print.discdd.predict}
\alias{print.discdd.predict}
%\alias{print}
\title{
    Printing results of discriminant analysis of discrete probability distributions
}
\description{
    \code{print} function, applied to an object of class \code{"discdd.predict"}, prints numerical results of \link{discdd.predict} .
}
\usage{
\method{print}{discdd.predict}(x, dist.print=TRUE, prox.print=FALSE, digits=2, ...)
}
\arguments{
  \item{x}{
      object of class \code{"discdd.predict"}, returned by \link{discdd.predict}.
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
    \item if available (if \code{misclass.ratio} argument of \code{\link{discdd.predict}} was \code{TRUE}), the whole misallocation ratio, the confusion matrix (allocations versus origins) and the misallocation ratio per class are printed.
    \item the data frame the rows of which are the groups, and the columns of which are of the origin (\code{NA} if not available) and allocation classes. 
    }
    
    If \code{dist.print = TRUE} or \code{prox.print = TRUE}, the distances or proximity indices between groups and classes, are displayed.
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
    \link{discdd.predict}; \link{print}.
}
\examples{
data(castles.dated)
data(castles.nondated)
stones <- rbind(castles.dated$stones, castles.nondated$stones)
periods <- rbind(castles.dated$periods, castles.nondated$periods)
stones$height <- cut(stones$height, breaks = c(19, 27, 40, 71), include.lowest = TRUE)
stones$width <- cut(stones$width, breaks = c(24, 45, 62, 144), include.lowest = TRUE)
stones$edging <- cut(stones$edging, breaks = c(0, 3, 4, 8), include.lowest = TRUE)
stones$boss <- cut(stones$boss, breaks = c(0, 6, 9, 20), include.lowest = TRUE )

castlesfh <- folderh(periods, "castle", stones)

result <- discdd.predict(castlesfh, "period")
print(result)
print(result, prox.print=TRUE)
}
