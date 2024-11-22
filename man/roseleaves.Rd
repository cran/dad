\name{roseleaves}
\alias{roseleaves}
\docType{data}
\title{
   Rose leaves
}
\description{
  The data are extracted from measures on roses from an agronomic experiment in a greenhouse and outdoors.
}
\usage{data("roseleaves")}
\format{
  \code{roseleaves} is a list of four data frames:
  \describe{
    \item{\code{roseflowers$rose}: }{data frame with 7 rows and 3 columns (factors) named
     \code{rose}, \code{place} and \code{variety}.}

    \item{\code{roseflowers$stem}: }{data frame with 12 rows and 5 columns named \code{rose}, \code{stem}, \code{date}, \code{order} (the ramification order of the stem) and \code{nleaves} (the number of leaves of the stem).}

    \item{\code{roseflowers$leaf}: }{data frame with 35 rows and 5 columns named \code{stem}, \code{leaf}, \code{rank} (the rank of the leaf on the stem), \code{nleaflets} and \code{lrachis} (the number of leaflets of the leaf and the length of its rachis).}

    \item{\code{roseflowers$leaflet}: }{data frame with 221 rows and 4 columns named \code{leaf}, \code{leaflet}, \code{lleaflet} and \code{wleaflet} (the length and width of the leaflet).}
}

  Each row (rose) in \code{roseleaves$rose} pertains to several rows (stems) in \code{roseleaves$stem}.

  Each row (stem) in \code{roseleaves$rose} pertains to several rows (leaves) in \code{roseleaves$leaf}.

  Each row (leaf) in \code{roseleaves$rose} pertains to several rows (leaflets) in \code{roseleaves$leaflet}.
}
\examples{
data(roseleaves)
summary(roseleaves$rose)
summary(roseleaves$stem)
summary(roseleaves$leaf)
summary(roseleaves$leaflet)
}
\keyword{datasets}
