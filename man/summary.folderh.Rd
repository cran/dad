\name{summary.folderh}
\alias{summary.folderh}
\title{
  Summarize a folderh
}
\description{
  Summarize an object of class \code{folderh}.
}
\usage{
\method{summary}{folderh}(object, ...)
}
\arguments{
  \item{object}{
     object of class \code{\link{folderh}} that is a list of data frames. These data frames have the same column names.
}
  \item{...}{
     further arguments passed to or from other methods.
}
}

\value{
 An object of class \code{summary.folderh}.
 
 An object of class \code{summary.folderh} is a list, each element of it containing the summary of the corresponding element of \code{object}.
 This object has an attribute \code{attr(, "keys")} (see \code{\link{folderh}}).
}

\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
\code{\link{folderh}}: object of class \code{folderh}.
}
\examples{
data(iris)

iris.fold <- as.folder(iris, "Species")
summary(iris.fold)
}                                                      
