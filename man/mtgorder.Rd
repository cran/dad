\name{mtgorder}
\alias{mtgorder}
\title{
  Branching order of vertices
}
\description{
  Computes the branching order of vertices contained in an object of class \code{\link{foldermtg}}. The order of a vertex is the number of the column of \code{\link{topology}}, which contains this vertex. 
}
\usage{
mtgorder(x, classes = "all", display = FALSE)
}
\arguments{
  \item{x}{
     an object of class \code{\link{foldermtg}}.
}
  \item{classes}{
     character vector. The classes of entities for which the branching order is computed. If omitted, the branching orders are computed for all entities.
}
  \item{display}{
     logical. If \code{TRUE}, the data frames of \code{x} corresponding to \code{classes} are displayed. Default: \code{FALSE}.
}
}
\details{
 Returns \code{x} after appending the branching orders of the vertices of the classes given in the argument \code{classes}. The branching orders 
 are appended to the data frames containing the vertices (one data frame per class) and the values of their corresponding features.
}
\value{
 Returns an object of class \code{\link{foldermtg}}, that is a list of data frames.
}

\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
\code{\link{read.mtg}}: reads a MTG file and builds an object of class \code{foldermtg}.

\code{\link{mtgorder}}.
}

\references{
    Cokelaer, T. and Pradal, C. (2010). \href{http://openalea.gforge.inria.fr/doc/vplants/newmtg/doc/_build/html/user/syntax.html}{MTG user guide}
}

\examples{
mtgfile <- system.file("extdata/plant1.mtg", package = "dad")
xmtg <- read.mtg(mtgfile)

# The branching orders
ymtg <- mtgorder(xmtg)
print(ymtg)

# Add the branching orders to the 'foldermtg'
zmtg <- mtgorder(xmtg, display = TRUE)
print(zmtg)
}
