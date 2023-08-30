\name{mtgrank}
\alias{mtgrank}
\title{
  Ranks of vertices in a decomposition
}
\description{
  Computes the rank of the vertices contained in an object of class \code{\link{foldermtg}}. The vertex sequences resulting from a decomposition of other vertices, the rank of the vertices making up the sequences are computed from the beginning of the sequence or from its end. These ranks can be absolute or relative.  
   
  For example: ranks of the phytomeres and inflorescences in each stem.
}
\usage{
mtgrank(x, classe, parent.class = NULL, sibling.classes = NULL,
  relative = FALSE, from = c("origin", "end"), rank.name = "Rank",
  display = FALSE)
}
\arguments{
  \item{x}{
     an object of class \code{\link{foldermtg}}.
}
  \item{classe}{
     character. The class of the vertices for which the ranks are computed.
}
  \item{parent.class}{
     character. The class of the parent entities of those for which the ranks are computed. If omitted, the entities of scale \code{maxscal - 1}, where \code{maxscal} is the highest scale in \code{x} data.
}
  \item{sibling.classes}{
     character vector. The classes of vertices appearing at the same scale as \code{classe}, which are used in the computing of the ranks.
     
     If omitted, only the vertices of class \code{classe} are used to compute the ranks.
}
  \item{relative}{
     logical. If \code{TRUE}, the relative ranks are computed, i.e. ranks from 0 to 1. Default: \code{FALSE}.
}
  \item{from}{
     character. It can be \code{"origin"} (default) or \code{"end"}.
     
     If \code{from = "origin"}, the ranks are computed from the origin to the end, i.e. from 1 to its maximum (from 0 to 1 if \code{relative = TRUE}). If \code{from = "end"}, they are computed from the end to the origin, i.e. from the maximum to 1 (from 1 to 0 if \code{relative = TRUE}).
}
  \item{rank.name}{
     character. Name of the rank column that is appended to \code{x[[classe]]}. The default is \code{"Rank"}.
}
  \item{display}{
     logical. If \code{TRUE}, the data frames of \code{x} corresponding to \code{classes} are displayed. Default: \code{FALSE}.
}
}
\details{
 If the branching orders of the entities given by \code{classe}, \code{parent.class} and, if relevant, \code{sibling.classes} are not contained in \code{x}, \code{mtgrank()} uses \code{\link{mtgorder}} to compute them. The ranks are appended to the data frames containing the vertices (one data frame per class) and the values of their corresponding features.
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
    Pradal, C., Godin, C. and Cokelaer, T. (2023). \href{https://mtg.readthedocs.io/en/latest/user/index.html}{MTG user guide}
}

\examples{
mtgfile <- system.file("extdata/plant1.mtg", package = "dad")
xmtg <- read.mtg(mtgfile)

ymtg <- mtgrank(xmtg, "M")
print(ymtg)

mtgrank(xmtg, "M", display = TRUE)

mtgrank(xmtg, "M", parent.class = "A", display = TRUE)
mtgrank(xmtg, "M", parent.class = "A", sibling.classes = c("O", "I"), display = TRUE)
mtgrank(xmtg, "M", relative = TRUE, display = TRUE)
mtgrank(xmtg, "M", from = "origin", display = TRUE)
mtgrank(xmtg, "M", from = "end", display = TRUE)
}
