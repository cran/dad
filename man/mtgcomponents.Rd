\name{mtgcomponents}
\alias{mtgcomponents}
\title{
  Components of upper scale of a vertex
}
\description{
  For a vertex in an object of class \code{\link{foldermtg}}, computes its decomposition into vertices of an upper scale.
}
\usage{
mtgcomponents(x, vertex, scale)
}
\arguments{
  \item{x}{
     an object of class \code{\link{foldermtg}}.
}
  \item{vertex}{
     character. The identifier of a vertex. These identifiers are the rownames of the data frame \code{x$topology}.
}
  \item{scale}{
     integer. The scale of the components of \code{vertex} which will be returned.
}
}
\details{
 If \code{vertex} is a vertex of scale \code{i}, then \code{scale} (the scale of the returned components of \code{vertex}) must be higher than \code{i}. For example, if \code{vertex} is a vertex of scale 2, then \code{scale > 2}, for instance \code{scale = 3}. The returned components are then vertices of scale 3 which have a decomposition relationship with \code{vertex}.
}
\value{
 A character vector, containing the idendifiers of the components of \code{vertex}.
 
 If there is no component, then the returned vector is empty.
}

\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
\code{\link{read.mtg}}: reads a MTG file and builds an object of class \code{foldermtg}.

\code{\link{mtgorder}}, \code{\link{mtgrank}}.
}

\references{
    Cokelaer, T. and Pradal, C. (2010). \href{http://openalea.gforge.inria.fr/doc/vplants/newmtg/doc/_build/html/user/tutorial_mtg_aml.html#components}{MTG user guide (Functions for moving in MTGs)}
}

\examples{
mtgfile <- system.file("extdata/plant1.mtg", package = "dad")
xmtg <- read.mtg(mtgfile)

# Vertex of class "P" (plant, of scale 1), components of class 2 (axes: "A")
mtgcomponents(xmtg, vertex = "v01", scale = 2)

# Vertex of class "P" (plant, of scale 1), components of class 3 ("O", "M" and "I")
mtgcomponents(xmtg, vertex = "v01", scale = 3)

# Vertex of class "A" (stem, of scale 2), components of class 3 ("O", "M" and "I")
mtgcomponents(xmtg, vertex = "v12", scale = 3)
}
