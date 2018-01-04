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
     object of class \code{\link{folderh}} that is a list of data frames.
}
  \item{...}{
     further arguments passed to or from other methods.
}
}

\value{
% An object of class \code{summary.folderh}.
% 
% An object of class \code{summary.folderh} is a
 A list, each element of it containing the summary of the corresponding element of \code{object}.
 This list has an attribute \code{attr(, "keys")} (see \code{\link{folderh}}).
}

\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
\code{\link{folderh}}: object of class \code{folderh}.
}
\examples{

# First example
mtgfile <- system.file("extdata/plant1.mtg", package = "dad")
x <- read.mtg(mtgfile)
fh1 <- as.folderh(x, classes = c("P", "A", "M"))
summary(fh1)

# Second example
data(roseleaves)
roses <- roseleaves$rose
stems <- roseleaves$stem
leaves <- roseleaves$leaf
leaflets <- roseleaves$leaflet
fh2 <- folderh(roses, "rose", stems, "stem", leaves, "leaf", leaflets)
summary(fh2)
}                                                      
