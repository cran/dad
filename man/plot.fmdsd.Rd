\name{plot.fmdsd}
\alias{plot.fmdsd}
%\alias{plot}

\title{
    Plotting scores of multidimensional scaling of density functions
}
\description{
    Applies to an object of class \code{fmdsd} (see the details section of the  
    \code{\link{fmdsd}} function). Plots the scores.
}
\usage{
\method{plot}{fmdsd}(x, nscore = 1:3, sub.title = NULL, ...)
}

\arguments{
  \item{x}{
       object of class \code{fmdsd}.
}
  \item{nscore}{
       numeric vector. The numbers of the score vectors to be plotted.
       
       Warning: Its components cannot be greater than the \code{nb.factors} argument in the call of the \code{\link{fmdsd}} function.
}
  \item{sub.title}{
       string. Subtitle to be added to each graph.
}
  \item{...}{
      optional arguments to \code{plot} methods.
}
}
\details{
    Plots the principal scores returned by the function \code{\link{fmdsd}}. 
    A new graphics window is opened for each pair of principal score vectors defined by the
    \code{nscore} argument.
}
\references{
    Boumaza, R., Yousfi, S., Demotes-Mainard, S. (2015). Interpreting the principal component analysis of multivariate density functions. Communications in Statistics - Theory and Methods, 44 (16), 3321-3339.
}
\author{
    Rachid Boumaza, Pierre Santagostini, Smail Yousfi, Sabine Demotes-Mainard.
}

\seealso{
    \link{fmdsd}; \link{print.fmdsd}; \link{interpret.fmdsd}.
}
\examples{
data(roses)
result = fmdsd(roses)
plot(result)
}
