\name{print.fpcad}
\alias{print.fpcad}
%\alias{print}
\title{
    Printing results of a functional PCA of probability densities
}
\description{
    Applies to an object of class \code{"fpcad"}. Prints the numeric results returned by the \code{\link{fpcad}} function.
}
\usage{
\method{print}{fpcad}(x, mean.print = FALSE, var.print = FALSE,
  cor.print = FALSE, skewness.print = FALSE, kurtosis.print = FALSE,
  digits = 2, ...)
}

\arguments{
  \item{x}{
      object of class \code{"fpcad"}, returned by the \code{\link{fpcad}} function.
}
  \item{mean.print}{
      logical. If \code{TRUE}, prints for each group the means and standard deviations of the variables and the norm of the density.
}
  \item{var.print}{
      logical. If \code{TRUE}, prints for each group the variances and covariances of the variables.
}
  \item{cor.print}{
      logical. If \code{TRUE}, prints for each group the correlations between the variables.
}
  \item{skewness.print}{
      logical. If \code{TRUE}, prints for each group the skewness coefficients of the variables.
}
  \item{kurtosis.print}{
      logical. If \code{TRUE}, prints for each group the kurtosis coefficients of the variables.
}
  \item{digits}{
      numeric. Number of significant digits for the display of numeric results.
}
  \item{...}{
      optional arguments to \code{print} methods.
}
}
\details{
    By default, are printed the inertia explained by the \code{nb.values} (see \code{\link{fpcad}}) first principal components, the contributions, the qualities of representation of the densities along the \code{nb.factors} (see \code{\link{fpcad}}) first principal components, and the principal scores.
}
\references{
    Boumaza, R., Yousfi, S., Demotes-Mainard, S. (2015). Interpreting the principal component analysis of multivariate density functions. Communications in Statistics - Theory and Methods, 44 (16), 3321-3339.
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
    \link{fpcad}; \link{plot.fpcad}; \link{interpret.fpcad}; \link{print}.
}
\examples{
data(roses)
rosefold <- as.folder(roses[,c("Sha","Den","Sym","rose")])
result <- fpcad(rosefold)
print(result)
print(result, mean.print = TRUE)
}
