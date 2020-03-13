\name{plot.hclustdd}
\alias{plot.hclustdd}
%\alias{plot}

\title{
    Plotting a hierarchical clustering of discrete distributions
}
\description{
    Applies to an object of class \code{hclustdd} (see details of the  
    \code{\link{hclustdd}} function). Plots the dendogram.
}
\usage{
\method{plot}{hclustdd}(x, labels = NULL, hang = 0.1, check = TRUE, axes = TRUE,
                        frame.plot = FALSE, ann = TRUE,
                        main = "HCA of probability density functions",
                        sub = NULL, xlab = NULL, ylab = "Height", ...)
}

\arguments{
  \item{x}{
       object of class \code{hclustdd} (returned by \code{\link{hclustdd}}).
}
  \item{labels, hang, check, axes, frame.plot, ann, main, sub, xlab, ylab}{
       Arguments concerning the graphical representation of the dendogram. See \code{\link{plot.hclust}}.
}
  \item{...}{
      Further graphical arguments.
}
}
%\references{
%
%}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
    \link{hclustdd}; \link{print.hclustdd}.
}
\examples{
data(dspg)
xl = dspg
result <- hclustdd(xl)
plot(result)
plot(result, hang = -1)
}
