\name{print.fmdsd}
\alias{print.fmdsd}
%\alias{print}
\title{
    Printing results of a multidimensional scaling analysis of probability densities
}
\description{
    Applies to an object of class \code{"fmdsd"}. Prints the numeric results returned by the \code{\link{fmdsd}} function.
}
\usage{
\method{print}{fmdsd}(x, mean.print = FALSE, var.print = FALSE,
  cor.print = FALSE, skewness.print = FALSE, kurtosis.print = FALSE,
  digits = 2, ...)
}

\arguments{
  \item{x}{
      object of class \code{"fmdsd"}, returned by the \code{\link{fmdsd}} function.
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
    By default, are printed the inertia explained by the \code{nb.values} (see \code{\link{fmdsd}}) first coordinates and the \code{nb.factors} (see \code{\link{fmdsd}}) coordinates of the densities.
}
\references{
    Boumaza, R., Yousfi, S., Demotes-Mainard, S. (2015). Interpreting the principal component analysis of multivariate density functions. Communications in Statistics - Theory and Methods, 44 (16), 3321-3339.
    }
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
    \link{fmdsd}; \link{plot.fmdsd}; \link{interpret.fmdsd}; \link{print}.
}
\examples{
data(roses)
x <- roses[,c("Sha","Den","Sym","rose")]
rosesfold <- as.folder(x)
result <- fmdsd(rosesfold)
print(result)
print(result, mean.print = TRUE)
}
