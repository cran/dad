\name{interpret.dstatis}
\alias{interpret.dstatis}

\title{
    Scores of the \code{dstatis} function vs. moments of the densities
}
\description{
    Applies to an object of class \code{"dstatis"}, plots the principal scores vs. the moments of the densities (means, standard deviations, variances, correlations, skewness and kurtosis coefficients), and computes the correlations between these scores and moments.
}
\usage{
\method{interpret}{dstatis}(x, nscore = 1, moment=c("mean", "sd", "var", "cov", "cor",
    "skewness", "kurtosis"), ...)
}
\arguments{
  \item{x}{
       object of class \code{"dstatis"} (returned by the \code{\link{dstatis.inter}} function).
}
  \item{nscore}{
       numeric. Selects the column of the data frame \code{x$scores} consisting of a score vector.
       
       Note that since dad-4, \code{nscore} can only be a single value (in earlier versions, it could be a vector of length > 1).
       
       Warning: \code{nscore} cannot be greater than the \code{nb.factors} argument in the call of the \code{\link{dstatis.inter}} function.
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
       }
}
  \item{...}{
       Arguments to be passed to methods.
}
}
\details{
    A graphics device can contain up to 9 graphs.
    If there are too many (more than 36) graphs for each score, one can display the graphs in a multipage PDF file.

    The number of principal scores to be interpreted cannot be greater than \code{nb.factors} of the data frame \code{x$scores} returned by the function \link{dstatis.inter}.
}
\value{
    Returns a list including:
    \item{pearson }{matrix of Pearson correlations between selected scores and moments.}
    \item{spearman }{matrix of Spearman correlations between selected scores and moments.}
}
\references{
    Lavit, C., Escoufier, Y., Sabatier, R., Traissac, P. (1994). The ACT (STATIS method). Computational Statistics & Data Analysis, 18 (1994), 97-119.
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
   \link{dstatis.inter}; \link{plot.dstatis}.
}
\examples{
data(roses)
rosesf <- as.folder(roses[,c("Sha","Den","Sym","rose")])

# Dual STATIS on the covariance matrices
\dontrun{
result <- dstatis.inter(rosesf, group.name = "rose")
interpret(result)
interpret(result, moment = "var")
interpret(result, moment = "cor")
interpret(result, nscore = 2)
}
}
