\name{plot.fhclustd}
\alias{plot.fhclustd}
%\alias{plot}

\title{
    Plotting a hierarchical clustering
}
\description{
    Applies to an object of class \code{fhclustd} (see details of the  
    \code{\link{fhclustd}} function). Plots the dendogram.
}
\usage{
\method{plot}{fhclustd}(x, labels = NULL, hang = 0.1, check = TRUE, axes = TRUE,
                        frame.plot = FALSE, ann = TRUE, main = "Cluster Dendrogram",
                        sub = NULL, xlab = NULL, ylab = "Height", ...)
}

\arguments{
  \item{x}{
       object of class \code{fhclustd} (returned by \code{\link{fhclustd}}).
}
  \item{labels, hang, check, axes, frame.plot, ann, main, sub, xlab, ylab}{
       Arguments concerning the graphical representation of the dendogram. See \code{\link{plot.hclust}}.
}
  \item{...}{
      Further graphical arguments. E.g., \code{cex} controls the size of the labels (if plotted) in the same way as \code{\link{text}}.
}
}
%\references{
%
%}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
    \link{fhclustd}; \link{print.fhclustd}.
}
\examples{
data(castles.dated)
xf <- as.folder(castles.dated$stones)
\dontrun{
result <- fhclustd(xf)
plot(result)
plot(result, hang = -1)
}
}
