\name{as.folder}
\alias{as.folder}
%\alias{as.folder.default}
\title{
  Coerce to a folder
}
\description{
  Coerces a data frame or an object of class \code{"folderh"} to an object of class \code{"folder"}.
}
\usage{
as.folder(x, ...)
}
\arguments{
  \item{x}{
     an object of class \code{data.frame} or \code{folderh}.
     \itemize{
     \item \code{data.frame}: see \code{\link{as.folder.data.frame}}
     \item \code{folderh}: see \code{\link{as.folder.folderh}}
     }
}
  \item{...}{
     further arguments passed to or from other methods.
}
}
\value{
 an object of class \code{\link{folder}}.
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
\code{\link{folder}}: objects of class \code{folder}.
\code{\link{as.data.frame.folder}}: build a data frame from an object of class \code{folder}.
\code{\link{as.folder.data.frame}}: build an object of class \code{folder} from a data frame.
\code{\link{as.folder.folderh}}: build an object of class \code{folder} from an object of class \code{folderh}.
}
