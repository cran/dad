\name{castles}
\alias{castles}
\docType{data}
\title{
   Alsacian castles by year of building
}
\description{
  The data were collected by J.M. Rudrauf on Alsacian castles whose building year is known (even approximatively).
  On each castle, he measured 4 structural parameters on a sample of building stones.
  
  These data are about the same castles as in \code{\link{castles.dated}} data set.
}
\usage{data(castles)}
\format{
  \code{castles} is a list of 46 data frames. Each of these data frames matches with one year (between 1136 and 1510) and contains measures on one or several castles which have been built since that year.
  
  Each data frame has 5 to 101 rows (stones) and 5 columns: \code{height}, \code{width}, \code{edging}, \code{boss} (numeric) and \code{castle} (factor).
}

\source{
    Rudrauf, J.M., Boumaza, R. (2001). Contribution a l'etude de l'architecture medievale: les caracteristiques des pierres a bossage des chateaux forts alsaciens. Centre de Recherches Archeologiques Medievales de Saverne, 5, 5-38.
}
\examples{
data(castles)
foldert(castles)
}
\keyword{datasets}
