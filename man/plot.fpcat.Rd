\name{plot.fpcat}
\alias{plot.fpcat}
%\alias{plot}

\title{
    Plotting scores of principal component analysis of density functions among time
}
\description{
    Applies to an object of class \code{fpcat} (see details of the  
    \code{\link{fpcat}} function). Plots the scores.
}
\usage{
\method{plot}{fpcat}(x, nscore = 1:3, sub.title = NULL, fontsize.points = 1.5, ...)
}

\arguments{
  \item{x}{
       object of class \code{fpcat} (returned by \code{\link{fpcat}}).
}
  \item{nscore}{
       numeric vector. The numbers of the score vectors to be plotted.
       
       Warning: Its components cannot be greater than the \code{nb.factors} argument in the call of the \code{\link{fpcat}} function.
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
   Plots:
   \itemize{
   \item{the principal scores returned by the \code{\link{fpcat}} function with arrows from the point corresponding to each time to the next one; a new graphics window is opened for each pair of principal axes defined by the \code{nscore} argument.}
   \item{the principal scores among time with arrows from each time to the next one; a new graphics window is opened for each principal axis defined by the \code{nscore} argument.}
   }
}
\references{
    Boumaza, R., Yousfi, S., Demotes-Mainard, S. (2015). Interpreting the principal component analysis of multivariate density functions. Communications in Statistics - Theory and Methods, 44 (16), 3321-3339.
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
    \link{fpcat}; \link{print.fpcat}%; \link{interpret.fpcat}.
}
\examples{
# First example: a normed non-centred PCA of Gaussian densities
# (data: stones of castles)
data(castles.dated)
stones <- castles.dated$stones
periods <- castles.dated$periods
period1 <- periods[periods$period == 1, "castle"]
castles1 <- stones[stones$castle \%in\% period1, 1:4]
period2 <- periods[periods$period == 2, "castle"]
castles2 <- stones[stones$castle \%in\% period2, 1:4]
period3 <- periods[periods$period == 3, "castle"]
castles3 <- stones[stones$castle \%in\% period3, 1:4]
period4 <- periods[periods$period == 4, "castle"]
castles4 <- stones[stones$castle \%in\% period4, 1:4]
period5 <- periods[periods$period == 5, "castle"]
castles5 <- stones[stones$castle \%in\% period5, 1:4]
period6 <- periods[periods$period == 6, "castle"]
castles6 <- stones[stones$castle \%in\% period6, 1:4]
castperiod <- foldert(list(castles1, castles2, castles3, castles4, castles5, castles6),
                      times = ordered(1:6))
fpcacastle <- fpcat(castperiod, group.name = "period")

plot(fpcacastle)

# Second example: the same castles, with their building year
data(castles)
castyear <- foldert(lapply(castles, "[", 1:4))
fpcayear <- fpcat(castyear, group.name = "year")

plot(fpcayear)
}
