\name{print.mdsdd}
\alias{print.mdsdd}
%\alias{print}
\title{
    Printing results of a multidimensional scaling analysis of discrete distributions
}
\description{
    Applies to an object of class \code{"mdsdd"}. Prints the numeric results returned by the \code{\link{mdsdd}} function.
}
\usage{
\method{print}{mdsdd}(x, joint = FALSE, margin1 = FALSE, margin2 = FALSE,
        association = FALSE, ...)
}

\arguments{
  \item{x}{
      object of class \code{"mdsdd"}, returned by the \code{\link{mdsdd}} function.
}
  \item{joint}{
      logical. If \code{TRUE}, prints for each group the table of estimated joint distribution.
}
  \item{margin1}{
      logical. If \code{TRUE}, prints for each group the data frame of estimated marginal distributions.
}
  \item{margin2}{
      logical. If \code{TRUE}, prints for each group the data frame of the estimated marginal distributions per combination of two variables.
}
  \item{association}{
      logical. If \code{TRUE}, prints for each group the matrix of the pairwise association measures of the variables.
}
  \item{...}{
      optional arguments to \code{print} methods.
}
}
\details{
    By default, are printed the inertia explained by the \code{nb.values} (see \code{\link{mdsdd}}) first coordinates and the \code{nb.factors} (see \code{\link{mdsdd}}) coordinates of the densities.
}
%\references{
%    
%    }
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Sabine Demotes-Mainard
}
\seealso{
    \link{mdsdd}; \link{plot.mdsdd}; \link{interpret.mdsdd}
}
\examples{
# INSEE (France): Diploma x Socio professional group, seven years.
data(dspg)
xlista = dspg
a <- mdsdd(xlista)
print(a, joint = TRUE, margin1 = TRUE, margin2 = TRUE) 
}