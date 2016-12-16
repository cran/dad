\name{plot.fpcad}
\alias{plot.fpcad}
%\alias{plot}

\title{
    Plotting scores of principal component analysis of density functions
}
\description{
    Applies to an object of class \code{fpcad} (see details of the  
    \code{\link{fpcad}} function). Plots the scores.
}
\usage{
\method{plot}{fpcad}(x, nscore = 1:3, sub.title = NULL, fontsize.points = 1.5, ...)
}

\arguments{
  \item{x}{
       object of class \code{fpcad} (returned by \code{\link{fpcad}}).
}
  \item{nscore}{
       numeric vector. The numbers of the score vectors to be plotted.
       
       Warning: Its components cannot be greater than the \code{nb.factors} argument in the call of the \code{\link{fpcad}} function.
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
   Plots the principal scores returned by the \code{\link{fpcad}} function. 
    A new graphics window is opened for each pair of principal axes defined by the \code{nscore} argument.
}
\references{
    Boumaza, R., Yousfi, S., Demotes-Mainard, S. (2015). Interpreting the principal component analysis of multivariate density functions. Communications in Statistics - Theory and Methods, 44 (16), 3321-3339.
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
    \link{fpcad}; \link{print.fpcad}; \link{interpret.fpcad}.
}
\examples{
data(roses)
rosefold <- as.folder(roses[,c("Sha","Den","Sym","rose")])
result <- fpcad(rosefold)
plot(result)
}
