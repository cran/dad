\name{mtgplant2}
\alias{mtgplant2}
\docType{data}
\title{
   Class \code{foldermtg}
}
\description{
  These data provides the topology of a bushy plant.
}
\usage{data("mtgplant2")}
\format{
  This object of class \code{\link{foldermtg}} is a list of 9 data frames:
  \itemize{
    \item{\code{mtgplant2$classes}: }{data frame with 6 rows and 5 columns named
     \code{SYMBOL} (factor: the classes of the vertices), \code{SCALE} (integer: the scale at which they appear), \code{DECOMPOSITION} (factor), \code{INDEXATION} (factor) and \code{DEFINITION} (factor).
     
     The vertex classes are:
       \itemize{
       \item \code{P}: the whole plant (scale 1)
       \item \code{A}: the axes (scale 2)
       \item \code{F}, \code{I}: the flower and internodes (scale 3)
       }
    }

    \item{\code{mtgplant2$description}: }{data frame with 4 rows and 4 columns (factors) named \code{LEFT}, \code{RIGHT}, \code{RELTYPE} and \code{MAX}.}

    \item{\code{mtgplant2$features}: }{data frame with 9 rows and 2 columns (factors) named \code{NAME} and \code{TYPE}.}

    \item{\code{mtgplant2$topology}: }{data frame with 14 rows and 3 columns:
      \itemize{
      \item \code{order1} and \code{order2} (factors): the codes of the vertices, as they are found in the MTG table of the MTG file. The column on which a code appears gives the branching order of the corresponding vertex.
      \item \code{vertex} (character): the same codes of vertices, on a single column.
      }
    }

    \item{\code{mtgplant2$coordinates}: }{data frame with 0 rows and 0 columns (there are no spatial coordinates in these MTG data).}

    \item{\code{mtgplant2$P}, \code{mtgplant2$A}, \code{mtgplant2$F} and \code{mtgplant2$I}: }{data frames of the features on the vertices (all numeric).}

  }
}
\details{
 This object of class \code{foldermtg} can be built by reading the data in a MTG file (see examples).
}

\seealso{
\code{\link{read.mtg}}: to read an MTG file and build an object of class MTG.

\code{\link{mtgplant1}}: an other example of such data.
}

\references{
    Cokelaer, T. and Pradal, C. (2010). \href{http://openalea.gforge.inria.fr/doc/vplants/newmtg/doc/_build/html/user/syntax.html}{MTG user guide}
}
\examples{
data(mtgplant2)
print(mtgplant2)

# To read these data from a MTG file:
mtgfile2 <- system.file("extdata/plant2.mtg", package = "dad")
mtgplant2 <- read.mtg(mtgfile2)
print(mtgplant2)
}
\keyword{datasets}
