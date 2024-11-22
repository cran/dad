\name{association measures}
\alias{cramer.data.frame}
\alias{tschuprow.data.frame}
\alias{pearson.data.frame}
\alias{phi.data.frame}
\title{
  Association measures between several categorical variables of a data frame
}
\description{
  Computes pairwise association measures (Cramer's V, Pearson's contingency coefficient, phi, Tschuprow's T) between the categorical variables of a data frame, using functions of the package \code{DescTools} (see \code{\link[DescTools]{Assocs}}).
}
\usage{
cramer.data.frame(x, check = TRUE)
pearson.data.frame(x, check = TRUE)
phi.data.frame(x, check = TRUE)
tschuprow.data.frame(x, check = TRUE)
}
\arguments{
  \item{x}{
     a data frame (can also be a tibble). Its columns should be factors.
}
  \item{check}{
     logical. If \code{TRUE} (default) the function checks if each column of \code{x} is a factor, and there is a warning if it is not.
}
}

\value{
 A square matrix whose elements are the pairwise association measures.
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Sabine Demotes-Mainard
}

\examples{
data(roses)
xr = roses[,c("Sha", "Den", "Sym", "rose")]
xr$Sha = cut(xr$Sha, breaks = c(0, 5, 7, 10))
xr$Den = cut(xr$Den, breaks = c(0, 4, 6, 10))
xr$Sym = cut(xr$Sym, breaks = c(0, 6, 8, 10))
cramer.data.frame(xr)
pearson.data.frame(xr)
phi.data.frame(xr)
tschuprow.data.frame(xr)
}