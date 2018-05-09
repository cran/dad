\name{as.foldert}
\alias{as.foldert}
%\alias{as.foldert.default}
\title{
  Coerce to a foldert
}
\description{
  Coerces a data frame or array to an object of class \code{\link{foldert}}.
}
\usage{
as.foldert(x, ...)
}
\arguments{
  \item{x}{
     an object of class \code{data.frame} or \code{array}.
     \itemize{
     \item \code{data.frame}: see \code{\link{as.foldert.data.frame}}
     \item \code{array}: see \code{\link{as.foldert.array}}
     }
}
  \item{...}{
     arguments passed to \code{\link{as.foldert.data.frame}} or \code{\link{as.foldert.array}},
     further arguments passed to or from other methods.
}
}
\value{
 an object of class \code{\link{foldert}}.
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}
