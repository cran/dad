\name{interpret}
\alias{interpret}

\title{
    Scores of the \code{fpcad}, \code{fpcat} or \code{fmdsd} function vs. moments of the densities
}
\description{
    This generic function provides a tool for the interpretation of the results of \code{fpcad}, \code{fpcat} or \code{fmdsd} function.
}
\usage{
interpret(x, nscore = 1:3, moment = "mean")
}
\arguments{
  \item{x}{
       object of class \code{fpcad}, \code{fpcat} or \code{fmdsd}.
       \itemize{
       \item \code{fpcad}: see \code{\link{interpret.fpcad}}
       \item \code{fpcat}: see \code{\link{interpret.fpcat}}
       \item \code{fmdsd}: see \code{\link{interpret.fmdsd}}
       }
}
  \item{nscore}{
       numeric vector. Selects the columns of the data frame \code{x$scores} to be interpreted.
       
       Warning: Its components cannot be greater than the \code{nb.factors} argument in the call of the \code{\link{fpcad}} or \code{\link{fpcat}} function.
}
  \item{moment}{
       characters string. Selects the moments to cross with scores:
       \itemize{
          \item \code{"mean"} (means)
          \item \code{"sd"} (standard deviations)
          \item \code{"cov"} (covariances)
          \item \code{"cor"} (correlation coefficients)
          \item \code{"skewness"} (skewness coefficients)
          \item \code{"kurtosis"} (kurtosis coefficients)
          \item \code{"all"} (for univariate densities only. It simultaneously considers means, standard deviations, variances and skewness and kurtosis coefficients)
       }
}
}
\value{
    Returns a list including:
    \item{pearson }{matrix of Pearson correlations between selected scores and moments.}
    \item{spearman }{matrix of Spearman correlations between selected scores and moments.}
}
\references{
    Boumaza, R., Yousfi, S., Demotes-Mainard, S. (2015). Interpreting the principal component analysis of multivariate density functions. Communications in Statistics - Theory and Methods, 44 (16), 3321-3339.
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
   \link{interpret.fpcad}; \link{interpret.fpcat}; \link{interpret.fmdsd}.
}
