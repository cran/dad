\name{mtgplant1}
\alias{mtgplant1}
\docType{data}
\title{
   Class \code{foldermtg}
}
\description{
  These data produced by the SAGAH team (Sciences Agronomiques Appliquées à l'Horticulture, now Research Institute on Horticulture and Seeds), provide the topological structure of a rosebush.
}
\usage{data("mtgplant1")}
\format{
  This object of class \code{\link{foldermtg}} is a list of 10 data frames:
  \itemize{
    \item{\code{mtgplant1$classes}: }{data frame with 6 rows and 5 columns named
     \code{SYMBOL} (factor: the classes of the vertices), \code{SCALE} (integer: the scale at which they appear), \code{DECOMPOSITION} (factor), \code{INDEXATION} (factor) and \code{DEFINITION} (factor).
     
     The vertex classes are:
       \itemize{
       \item \code{P}: the whole plant (scale 1)
       \item \code{A}: the axes (scale 2)
       \item \code{O}, \code{M}, \code{I}: the ..., metamers (phytomers) and inflorescences (scale 3)
       }
    }

    \item{\code{mtgplant1$description}: }{data frame with 8 rows and 4 columns (factors) named \code{LEFT}, \code{RIGHT}, \code{RELTYPE} and \code{MAX}.}

    \item{\code{mtgplant1$features}: }{data frame with 13 rows and 2 columns (factors) named \code{NAME} and \code{TYPE}.}

    \item{\code{mtgplant1$topology}: }{data frame with 88 rows and 4 columns:
      \itemize{
      \item \code{order1}, \code{order2} and \code{order3} (factors): the codes of the vertices, as they are found in the MTG table of the MTG file. The column on which a code appears gives the branching order of the corresponding vertex.
      \item \code{vertex} (character): the same codes of vertices, on a single column.
      }
    }

    \item{\code{mtgplant1$coordinates}: }{data frame with 86 rows and 6 columns (numeric) named \code{XX}, \code{YY} and \code{22}: cartesian coordinates of the vertices, and \code{AA}, \code{BB} and \code{CC}: an other coordinates system.}

    \item{\code{mtgplant1$P}, \code{mtgplant1$A}, \code{mtgplant1$M}, \code{mtgplant1$I}: }{data frames of the features on the vertices (all numeric).}

  }
}
\details{
 This object of class \code{foldermtg} can be built by reading the data in a MTG file (see examples).
}

\seealso{
\code{\link{read.mtg}}: to read an MTG file and build an object of class MTG.

\code{\link{mtgplant2}}: an other example of such data.
}

\references{
    Cokelaer, T. and Pradal, C. (2010). \href{http://openalea.gforge.inria.fr/doc/vplants/newmtg/doc/_build/html/user/syntax.html}{MTG user guide}
}
\examples{
data(mtgplant1)
print(mtgplant1)

# To read these data from a MTG file:
mtgfile1 <- system.file("extdata/plant1.mtg", package = "dad")
mtgplant1 <- read.mtg(mtgfile1)
print(mtgplant1)
}
\keyword{datasets}
