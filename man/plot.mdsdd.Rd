\name{plot.mdsdd}
\alias{plot.mdsdd}
%\alias{plot}

\title{
    Plotting scores of multidimensional scaling analysis of discrete distributions
}
\description{
    Applies to an object of class \code{"mdsdd"} (see the details section of the  
    \code{\link{mdsdd}} function). Plots the scores.
}
\usage{
\method{plot}{mdsdd}(x, nscore = 1:2, sub.title = NULL, fontsize.points = 1.5, ...)
}

\arguments{
  \item{x}{
       object of class \code{mdsdd}.
}
  \item{nscore}{
       numeric vector. The numbers of the score vectors to be plotted.
       
       Warning: Its components cannot be greater than the \code{nb.factors} argument in the call of the \code{\link{fmdsd}} function.
}
  \item{sub.title}{
       string. Subtitle to be added to each graph.
}
  \item{fontsize.points}{
       Numeric. Expansion of the characters (or symbols) of the groups on the graph. This works as a multiple of \code{par("cex")} (see \code{\link{points}}).
}
  \item{...}{
      optional arguments to \code{plot} methods.
}
}
\details{
    Plots the principal scores returned by the function \code{\link{mdsdd}}. 
    A new graphics window is opened for each pair of principal score vectors defined by the
    \code{nscore} argument.
}
%\references{
%    
%    }
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Sabine Demotes-Mainard
}

\seealso{
    \link{mdsdd}; \link{print.mdsdd}; \link{interpret.mdsdd}.
}
\examples{
# INSEE (France): Diploma x Socio professional group, seven years.
data(dspg)
xlista = dspg
a <- mdsdd(xlista)
plot(a) 
}
