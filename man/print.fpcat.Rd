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
# First example: a normed non-centred PCA of Gaussian densities
# (data: stones of castles)
data(castles.dated)
stones <- castles.dated$stones
periods <- castles.dated$periods
period1 <- periods[periods$period == 1, "castle"]
castles1 <- stones[stones$castle \%in\% period1, 1:4]
period2 <- periods[periods$period == 2, "castle"]
castles2 <- stones[stones$castle \%in\% period2, 1:4]
period3 <- periods[periods$period == 3, "castle"]
castles3 <- stones[stones$castle \%in\% period3, 1:4]
period4 <- periods[periods$period == 4, "castle"]
castles4 <- stones[stones$castle \%in\% period4, 1:4]
period5 <- periods[periods$period == 5, "castle"]
castles5 <- stones[stones$castle \%in\% period5, 1:4]
period6 <- periods[periods$period == 6, "castle"]
castles6 <- stones[stones$castle \%in\% period6, 1:4]
castperiod <- foldert(list(castles1, castles2, castles3, castles4, castles5, castles6),
                      times = ordered(1:6))
fpcacastle <- fpcat(castperiod, group.name = "period")

print(fpcacastle)
print(fpcacastle, mean.print = TRUE, var.print = TRUE)

# Second example: the same castles, with their building year
data(castles)
castyear <- foldert(lapply(castles, "[", 1:4))
fpcayear <- fpcat(castyear, group.name = "year")

print(fpcayear)
print(fpcayear, mean.print = TRUE, var.print = TRUE)
}
