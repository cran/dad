\name{interpret.fmdsd}
\alias{interpret.fmdsd}

\title{
    Scores of the \code{fmdsd} function vs. moments of the densities
}
\description{
    Applies to an object of class \code{"fmdsd"}, plots the scores vs. the moments of the densities (means, standard deviations, variances, correlations, skewness and kurtosis coefficients), and computes the correlations between these scores and moments.
}
\usage{
\method{interpret}{fmdsd}(x, nscore = 1, moment=c("mean", "sd", "var", "cov", "cor",
    "skewness", "kurtosis"), ...)
}
\arguments{
  \item{x}{
       object of class \code{"fmdsd"} (returned by the \code{\link{fmdsd}} function).
}
  \item{nscore}{
       numeric. Selects the column of the data frame \code{x$scores} consisting of a score vector.
       
       Note that since dad-4, \code{nscore} can only be a single value (in earlier versions, it could be a vector of length > 1).
       
       Warning: \code{nscore} cannot be greater than the \code{nb.factors} argument in the call of the \code{\link{fmdsd}} function.
}
  \item{moment}{
       character string. Selects the moments to cross with scores:
       \itemize{
          \item \code{"mean"} (means, which is the default value)
          \item \code{"sd"} (standard deviations)
          \item \code{"cov"} (covariances)
          \item \code{"cor"} (correlation coefficients)
          \item \code{"skewness"} (skewness coefficients)
          \item \code{"kurtosis"} (kurtosis coefficients)
       }
}
  \item{...}{
       Arguments to be passed to methods.
}
}
\details{
    A graphics device can contain up to 9 graphs.
    If there are too many (more than 36) graphs for each score, one can display the graphs in a multipage PDF file.

    The number of principal scores to be interpreted cannot be greater than \code{nb.factors} of the data frame \code{x$scores} returned by the function \code{\link{fmdsd}}.
}
\value{
    Returns a list including:
    \item{pearson }{matrix of Pearson correlations between selected scores and moments.}
    \item{spearman }{matrix of Spearman correlations between selected scores and moments.}
}
\references{
    Boumaza, R., Yousfi, S., Demotes-Mainard, S. (2015). Interpreting the principal component analysis of multivariate density functions. Communications in Statistics - Theory and Methods, 44 (16), 3321-3339.
    
    Delicado, P. (2011). Dimensionality reduction when data are density functions. Computational Statistics & Data Analysis, 55, 401-420.
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
   \link{fmdsd}; \link{plot.fmdsd}.
}
\examples{
data(roses)
x <- roses[,c("Sha","Den","Sym","rose")]
rosesfold <- as.folder(x)
result1 <- fmdsd(rosesfold)
interpret(result1)
\dontrun{
interpret(result1, moment = "var")
}
interpret(result1, nscore = 2)
}
