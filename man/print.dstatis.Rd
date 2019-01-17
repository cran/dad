\name{print.dstatis}
\alias{print.dstatis}
%\alias{print}
\title{
    Printing results of STATIS method (interstructure) analysis
}
\description{
    Applies to an object of class \code{"dstatis"}. Prints the numeric results returned by the \code{\link{dstatis.inter}} function.
}
\usage{
\method{print}{dstatis}(x, mean.print = FALSE, var.print = FALSE,
  cor.print = FALSE, skewness.print = FALSE, kurtosis.print = FALSE,
  digits = 2, ...)
}

\arguments{
  \item{x}{
      object of class \code{"dstatis"}, returned by the \code{\link{dstatis.inter}} function.
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
    By default, are printed the inertia explained by the \code{nb.values} (see \code{\link{dstatis.inter}}) first principal components, the contributions, the qualities of representation of the densities along the \code{nb.factors} (see \code{\link{dstatis.inter}}) first principal components, and the principal scores.
}
\references{
    Lavit, C., Escoufier, Y., Sabatier, R., Traissac, P. (1994). The ACT (STATIS method). Computational Statistics & Data Analysis, 18 (1994), 97-119.
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
    \link{dstatis.inter}; \link{plot.dstatis}; \link{interpret.dstatis}; \link{print.dstatis}.
}
\examples{
data(roses)
rosesf <- as.folder(roses[,c("Sha","Den","Sym","rose")])

# Dual STATIS on the covariance matrices
result <- dstatis.inter(rosesf, data.scaled = FALSE, group.name = "rose")
print(result)
}
