\name{interpret}
\alias{interpret}

\title{
    Scores of \code{fmdsd}, \code{dstatis}, \code{fpcad}, or \code{fpcat} vs. moments, or scores of \code{mdsdd} vs. marginal distributions or association measures
}
\description{
    This generic function provides a tool for the interpretation of the results of \code{fmdsd}, \code{dstatis}, \code{fpcad}, \code{fpcat} or \code{mdsdd} function.
}
\usage{
interpret(x, nscore = 1:3, ...)
}
\arguments{
  \item{x}{
       object of class \code{fmdsd}, \code{dstatis}, \code{fpcad}, \code{fpcat} or \code{mdsdd}.
       \itemize{
       \item \code{fmdsd}: see \code{\link{interpret.fmdsd}}
       \item \code{dstatis}: see \code{\link{interpret.dstatis}}
       \item \code{fpcad}: see \code{\link{interpret.fpcad}}
       \item \code{fpcat}: see \code{\link{interpret.fpcat}}
       \item \code{mdsdd}: see \code{\link{interpret.mdsdd}}
       }
}
  \item{nscore}{
       numeric vector. Selects the columns of the data frame \code{x$scores} to be interpreted.
       
       Warning: Its components cannot be greater than the \code{nb.factors} argument in the call of the \code{\link{fpcad}} or \code{\link{fpcat}} function.
}
  \item{...}{
       Arguments to be passed to the methods, such as \code{moment} (for \code{\link{interpret.fmdsd}}, \code{\link{interpret.dstatis}},
       \code{\link{interpret.fpcad}} and \code{\link{interpret.fpcat}}), or \code{mma} (for \code{\link{interpret.mdsdd}}).
}
}
\value{
    Returns a list including:
    \item{pearson }{matrix of Pearson correlations between selected scores and moments, probabilities or associations.}
    \item{spearman }{matrix of Spearman correlations between selected scores and moments, probabilities or associations.}
}
\references{
    Boumaza, R., Yousfi, S., Demotes-Mainard, S. (2015). Interpreting the principal component analysis of multivariate density functions. Communications in Statistics - Theory and Methods, 44 (16), 3321-3339.
}
\author{
Rachid Boumaza, Pierre Santagostini, Smail Yousfi, Sabine Demotes-Mainard
}

\seealso{
   \link{interpret.fmdsd}; \link{interpret.dstatis}; \link{interpret.fpcad}; \link{interpret.fpcat}; \link{interpret.mdsdd}.
}
