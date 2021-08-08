\name{plot.fmdsd}
\alias{plot.fmdsd}
%\alias{plot}

\title{
    Plotting scores of multidimensional scaling of density functions
}
\description{
    Applies to an object of class \code{"fmdsd"} (see the details section of the  
    \code{\link{fmdsd}} function). Plots the scores.
}
\usage{
\method{plot}{fmdsd}(x, nscore = c(1, 2), main="MDS of probability density functions",
    sub.title = NULL, color = NULL, fontsize.points = 1.5, ...)
}

\arguments{
  \item{x}{
       object of class \code{"fmdsd"}.
}
  \item{nscore}{
       a length 2 numeric vector. The numbers of the score vectors to be plotted.
       
       Warning: Its components cannot be greater than the \code{nb.factors} argument in the call of the \code{\link{fmdsd}} function.
}
  \item{main}{
       this argument to title has an useful default here.
}
  \item{sub.title}{
       string. Subtitle to be added to each graph.
}
  \item{color}{
       When provided, the colour of the symbols of each group. Can be a vector with length equal to the number of groups.
}
  \item{fontsize.points}{
       Numeric. Expansion of the characters (or symbols) of the groups on the graph. This works as a multiple of \code{par("cex")} (see \code{\link{points}}).
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
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
    \link{fmdsd}; \link{print.fmdsd}; \link{interpret.fmdsd}.
}
\examples{
data(roses)
x <- roses[,c("Sha","Den","Sym","rose")]
rosesfold <- as.folder(x)
result <- fmdsd(rosesfold)
plot(result)
}
