\name{as.folderh}
\alias{as.folderh}
%\alias{as.folder.default}
\title{
  Coerce to a folderh
}
\description{
  Coerces an object to an object of class \code{\link{folderh}}.
}
\usage{
as.folderh(x, classes)
}
\arguments{
  \item{x}{
     an object to be coerced to an object of class \code{folderh}. In the current version, it is an object of class \code{"foldermtg"} (see \code{\link{as.folderh.foldermtg}}).
     }
  \item{classes}{
     argument useful for \code{\link{as.folderh.foldermtg}}.
     }
}
\value{
 an object of class \code{\link{folderh}}.
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
\code{\link{as.folderh.foldermtg}}: build an object of class \code{folderh} from an object of class \code{foldermtg}.
}
