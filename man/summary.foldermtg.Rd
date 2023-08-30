\name{summary.foldermtg}
\alias{summary.foldermtg}
%\alias{summary}
\title{
    Summary of an object of class \code{foldermtg}
}
\description{
    Summary method for S3 class \code{foldermtg}.
}
\usage{
\method{summary}{foldermtg}(object, ...)
}
\arguments{
  \item{object}{
     an object of class \code{\link{foldermtg}}.
}
  \item{...}{
      optional arguments to \code{summary} methods.
}
}
\value{
    The summary of the data frames containing the vertices of each class and the values of the features on these vertices.
}
\references{
    Pradal, C., Godin, C. and Cokelaer, T. (2023). \href{https://mtg.readthedocs.io/en/latest/user/index.html}{MTG user guide}
}

\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
\code{\link{read.mtg}}: reads a MTG file and creates an object of class \code{"foldermtg"}.
}
\examples{
mtgfile1 <- system.file("extdata/plant1.mtg", package = "dad")
x1 <- read.mtg(mtgfile1)
summary(x1)

mtgfile2 <- system.file("extdata/plant2.mtg", package = "dad")
x2 <- read.mtg(mtgfile2)
summary(x2)
}
