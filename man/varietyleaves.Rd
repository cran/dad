\name{varietyleaves}
\alias{varietyleaves}
\docType{data}
\title{
   Rose variety leaves
}
\description{
  The data are extracted from measures on roses from an agronomic experiment in a greenhouse and outdoors.
}
\usage{data("varietyleaves")}
\format{
  \code{varietyleaves} is an object of class \code{"folderh"}, that is a list of two data frames:
  \describe{
    \item{\code{varietyleaves$variety}: }{data frame with 31 rows and 2 columns (factors) named
     \code{rose} and \code{variety}.}

    \item{\code{varietyleaves$leaves}: }{data frame with 581 rows and 5 columns named \code{rose}, \code{nleaflet} (number of leaflets), \code{lrachis} (length of the rachis), \code{lleaflet} (length of the principal leaflet) and \code{wleaflet} (width of the principal leaflet).}
}
}
\examples{
data(varietyleaves)
summary(varietyleaves)
}
\keyword{datasets}
