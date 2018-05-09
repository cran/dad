\name{castles.nondated}
\alias{castles.nondated}
\docType{data}
\title{
   Non dated Alsacian castles
}
\description{
  The data were collected by J.M. Rudrauf on Alsacian castles whose building period is unknown.
  On each castle, he measured 4 structural parameters on a sample of building stones. 
}
\usage{data(castles.nondated)}
\format{
  \code{castles.nondated} is a list of two data frames:
  
  \itemize{
    \item{\code{castles.nondated$stones}: }{this first data frame has 1280 cases (rows) and 5 variables (columns) that are named
     \code{height, width, edging, boss} (numeric) and \code{castle} (factor).}

    \item{\code{castles.nondated$periods}: }{this second data frame has 67 cases and 2 variables named \code{castle} and \code{period}; the column \code{castle} corresponds to the levels of the factor \code{castle} of the first data frame; the column \code{period} is a factor indicating NA as the building period is unknown.}
}
  Notice that the data frames corresponding to the castles whose building period is known are those in \code{\link{castles.dated}}.
}
\source{
    Rudrauf, J.M., Boumaza, R. (2001). Contribution à l'étude de l'architecture médiévale: les caractéristiques des pierres à bossage des châteaux forts alsaciens. Centre de Recherches Archéologiques Médiévales de Saverne, 5, 5-38.
}

\examples{
data(castles.nondated)
summary(castles.nondated$stones)
summary(castles.nondated$periods)
}
\keyword{datasets}
