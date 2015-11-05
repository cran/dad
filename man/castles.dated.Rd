\name{castles.dated}
\alias{castles.dated}
\docType{data}
\title{
   Dated castles
}
\description{
  The data were collected by J.M. Rudrauf on Alsacian castles whose building period is
  known (even approximatively).
  On each castle, he measured 4 structural parameters on a sample of building stones.
}
\usage{data(castles.dated)}
\format{
  \code{castles.dated} is a list of two data frames:
  
  \itemize{
    \item{\code{castles.dated$stones}: }{this first data frame has 1262 cases (rows) and 5 variables (columns) that are named
     \code{height, width, eidgen, boss} (numeric) and \code{castle} (factor).}

    \item{\code{castles.dated$periods}: }{this second data frame has 68 cases and 2 variables named \code{castle} and \code{period}; the column \code{castle} corresponds to the levels of the factor \code{castle} of the first data frame; the column \code{period} is a factor with 6 levels indicating the approximative building period. Thus this factor defines 6 classes of castles.}
}
}

\source{
    Rudrauf, J.M., Boumaza, R. (2001). Contribution à l'étude de l'architecture médiévale: les caractéristiques des pierres à bossage des châteaux forts alsaciens. Centre de Recherches Archéologiques Médiévales de Saverne, 5, 5-38.
}
\examples{
data(castles.dated)
summary(castles.dated$stones)
summary(castles.dated$periods)
}
\keyword{datasets}
