\name{interpret.fpcad}
\alias{interpret.fpcad}

\title{
    Scores of the \code{fpcad} function vs. moments of the densities
}
\description{
    Applies to an object of class \code{fpcad}, plots the principal scores vs. the moments of the densities (means, standard deviations, variances, correlations, skewness and kurtosis coefficients), and computes the correlations between these scores and moments.
}
\usage{
\method{interpret}{fpcad}(x, nscore = 1:3, moment = "mean")
}
\arguments{
  \item{x}{
       object of class \code{fpcad} (returned by the \code{\link{fpcad}} function).
}
  \item{nscore}{
       numeric vector. Selects the columns of the data frame \code{x$scores} to be interpreted.
       
       Warning: Its components cannot be greater than the \code{nb.factors} argument in the call of the \code{\link{fpcad}} function.
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
\details{
    A new graphics device is opened for each score column. A device can contain up to 9 graphs.
    If there are too many (more than 36) graphs for each score, a multipage PDF file is created in the current working directory, and the graphs are displayed in it.

    The number of principal scores to be interpreted cannot be greater than \code{nb.factors} of the data frame \code{x$scores} returned by the function \link{fpcad}.

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
   \link{fpcad}; \link{plot.fpcad}.
}
\examples{
data(roses)
rosefold <- as.folder(roses[,c("Sha","Den","Sym","rose")])
result1 <- fpcad(rosefold)
interpret(result1)
interpret(result1, moment = "var")
interpret(result1, moment = "cor")
interpret(result1, nscore = 1:2)
}
