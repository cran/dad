\name{roseflowers}
\alias{roseflowers}
\docType{data}
\title{
   Rose flowers
}
\description{
  The data are extracted from measures on roses from an agronomic experiment in a greenhouse and outdoors.
}
\usage{data(roseflowers)}
\format{
  \code{roseflowers} is a list of two data frames:
  
  \itemize{
    \item{\code{roseflowers$variety}: }{this first data frame has 5 rows and 3 columns (factors) named
     \code{place}, \code{numrose} and \code{variety}.}

    \item{\code{roseflowers$flower}: }{this second data frame has 11 cases and 5 columns named \code{numflower} (the order number of the flower), \code{rose}, \code{diameter} and \code{height} (the diameter and height of the flower), and \code{nleaves} (the number of the leaves of the axis).}
}
}
\examples{
data(roseflowers)
summary(roseflowers$variety)
summary(roseflowers$flower)
}
\keyword{datasets}
