\name{plot.dstatis}
\alias{plot.dstatis}
%\alias{plot}

\title{
    Plotting scores of STATIS method (interstructure) analysis
}
\description{
    Applies to an object of class \code{dstatis} (see details of the  
    \code{\link{dstatis.inter}} function). Plots the scores.
}
\usage{
\method{plot}{dstatis}(x, nscore = 1:3, sub.title = NULL, fontsize.points = 1.5, ...)
}

\arguments{
  \item{x}{
       object of class \code{dstatis} (returned by \code{\link{dstatis.inter}}).
}
  \item{nscore}{
       numeric vector. The numbers of the score vectors to be plotted.
       
       Warning: Its components cannot be greater than the \code{nb.factors} argument in the call of the \code{\link{dstatis.inter}} function.
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
   Plots the principal scores returned by the \code{\link{dstatis.inter}} function. 
    A new graphics window is opened for each pair of principal axes defined by the \code{nscore} argument.
}
\references{
    Lavit, C., Escoufier, Y., Sabatier, R., Traissac, P. (1994). The ACT (STATIS method). Computational Statistics & Data Analysis, 18 (1994), 97-119.
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
    \link{dstatis.inter}; \link{print.dstatis}; \link{interpret.dstatis}.
}
\examples{
data(roses)
rosesf <- as.folder(roses[,c("Sha","Den","Sym","rose")])

# Dual STATIS on the covariance matrices
result <- dstatis.inter(rosesf, data.scaled = FALSE, group.name = "rose")
plot(result)
}
