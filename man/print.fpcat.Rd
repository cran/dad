\name{print.fpcat}
\alias{print.fpcat}
%\alias{print}
\title{
    Printing results of a functional PCA of probability densities among time
}
\description{
    Applies to an object of class \code{"fpcat"}. Prints the numeric results returned by the \code{\link{fpcat}} function.
}
\usage{
\method{print}{fpcat}(x, mean.print = FALSE, var.print = FALSE,
  cor.print = FALSE, skewness.print = FALSE, kurtosis.print = FALSE,
  digits = 2, ...)
}

\arguments{
  \item{x}{
      object of class \code{"fpcat"}, returned by the \code{\link{fpcat}} function.
}
  \item{mean.print}{
      logical. If \code{TRUE}, prints for each observation time the means and standard deviations of the variables and the norm of the density.
}
  \item{var.print}{
      logical. If \code{TRUE}, prints for each observation time the variances and covariances of the variables.
}
  \item{cor.print}{
      logical. If \code{TRUE}, prints for each observation time the correlations between the variables.
}
  \item{skewness.print}{
      logical. If \code{TRUE}, prints for each observation time the skewness coefficients of the variables.
}
  \item{kurtosis.print}{
      logical. If \code{TRUE}, prints for each observation time the kurtosis coefficients of the variables.
}
  \item{digits}{
      numeric. Number of significant digits for the display of numeric results.
}
  \item{...}{
      optional arguments to \code{print} methods.
}
}
\details{
    By default, are printed the vector of observation times (numeric, ordered factor or object of class \code{"Date"}), the inertia explained by the \code{nb.values} (see \code{\link{fpcat}}) first principal components, the contributions, the qualities of representation of the densities along the \code{nb.factors} (see \code{\link{fpcat}}) first principal components, and the principal scores.
}
\references{
    Boumaza, R., Yousfi, S., Demotes-Mainard, S. (2015). Interpreting the principal component analysis of multivariate density functions. Communications in Statistics - Theory and Methods, 44 (16), 3321-3339.
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
    \link{fpcat}; \link{plot.fpcat}; \link{print}.
}
\examples{
times <- as.Date(c("2017-03-01", "2017-04-01", "2017-05-01", "2017-06-01"))
x1 <- data.frame(z1=rnorm(6,1,5), z2=rnorm(6,3,3))
x2 <- data.frame(z1=rnorm(6,4,6), z2=rnorm(6,5,2))
x3 <- data.frame(z1=rnorm(6,7,2), z2=rnorm(6,8,4))
x4 <- data.frame(z1=rnorm(6,9,3), z2=rnorm(6,10,2))
ft <- foldert(x1, x2, x3, x4, times = times, rows.select="intersect")
print(ft)
result <- fpcat(ft)

print(result)
print(result, mean.print = TRUE, var.print = TRUE)
}
