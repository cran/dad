\name{print.foldermtg}
\alias{print.foldermtg}
%\alias{print}
\title{
    Printing an object of class \code{foldermtg}
}
\description{
    \code{print} function, applied to an object of class \code{"foldermtg"}, prints an MTG (Multiscale Tree Graph) folder, as returned by \code{\link{foldermtg}} function.
}
\usage{
\method{print}{foldermtg}(x, classes = TRUE, description = FALSE, features = TRUE,
  topology = FALSE, coordinates = FALSE, ...)
}
\arguments{
  \item{x}{
     an object of class \code{\link{foldermtg}}.
}
  \item{classes}{
      logical. If \code{TRUE} (default), prints the data frame describing the classes (\code{CLASSES:} table in the MTG file).
}
  \item{description}{
      logical. If \code{TRUE} (default: \code{FALSE}), prints the description data frame (\code{DESCRIPTION:} table in the MTG file).
}
  \item{features}{
      logical. If \code{TRUE} (default), prints the data frame of the features and their types (\code{FEATURES:} table in the MTG file).
}
  \item{topology}{
      logical. If \code{TRUE} (default: \code{FALSE}), prints the data frame of the plant topology.
}
  \item{coordinates}{
      logical. If \code{TRUE} (default: \code{FALSE}), prints the spatial coordinates of the entities of the plant.
}
  \item{...}{
      optional arguments to \code{print} methods.
}
}
\details{
    If \code{classes}, \code{description} or \code{features} are \code{TRUE}, the corresponding data frames are displayed.
    
    If \code{topology = TRUE}, the plant structure is displayed; and if  \code{coordinates = TRUE}, the spatial coordinates are displayed.
    
    By default, the data frames containing the features on the vertices per class are printed.
}
\references{
    Cokelaer, T. and Pradal, C. (2010). \href{http://openalea.gforge.inria.fr/doc/vplants/newmtg/doc/_build/html/user/syntax.html}{MTG user guide}
}

\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
\code{\link{read.mtg}}: reads a MTG file and creates an object of class \code{"foldermtg"}.
}
\examples{
mtgfile1 <- system.file("extdata/plant1.mtg", package = "dad")
xmtg1 <- read.mtg(mtgfile1)
print(xmtg1)
print(xmtg1, topology = FALSE)
print(xmtg1, coordinates = TRUE)

mtgfile2 <- system.file("extdata/plant2.mtg", package = "dad")
xmtg2 <- read.mtg(mtgfile2)
print(xmtg2)
print(xmtg2, topology = FALSE)
print(xmtg2, coordinates = TRUE)
}
