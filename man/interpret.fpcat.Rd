\name{interpret.fpcat}
\alias{interpret.fpcat}

\title{
    Scores of the \code{fpcat} function vs. moments of the densities
}
\description{
    This function applies to an object of class \code{fpcat} and does the same as for an object of class \code{fpcad}: it plots the principal scores vs. the moments of the densities (means, standard deviations, variances, correlations, skewness and kurtosis coefficients), and computes the correlations between these scores and moments.
    
    In the next version of the package, this function will be updated by some more graphics.
}
\usage{
\method{interpret}{fpcat}(x, nscore = 1:3, moment=c("mean", "sd", "var", "cov", "cor",
    "skewness", "kurtosis", "all"), ...)
}
\arguments{
  \item{x}{
       object of class \code{fpcat} (returned by the \code{\link{fpcat}} function).
}
  \item{nscore}{
       numeric vector. Selects the columns of the data frame \code{x$scores} to be interpreted.
       
       Warning: Its components cannot be greater than the \code{nb.factors} argument in the call of the \code{\link{fpcat}} function.
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
  \item{...}{
       Arguments to be passed to methods.
}
}
\details{
    A new graphics device is opened for each score column. A device can contain up to 9 graphs.
    If there are too many (more than 36) graphs for each score, a multipage PDF file is created in the current working directory, and the graphs are displayed in it.

    The number of principal scores to be interpreted cannot be greater than \code{nb.factors} of the data frame \code{x$scores} returned by the function \link{fpcat}.

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
   \link{fpcat}; \link{plot.fpcat}.
}
\examples{
# Alsacian castles with their building year
data(castles)
castyear <- foldert(lapply(castles, "[", 1:4))
fpcayear <- fpcat(castyear, group.name = "year")
interpret(fpcayear)
\dontrun{
interpret(fpcayear, moment="var")
}
}
