\name{rosephytomer}
\alias{rosephytomer}
\docType{data}
\title{
Rose leaf and internode dynamics
}
\description{
These data are extracted from measures on rosebushes during a study on leaf and internode expansion dynamics.
For four rosebushes, on each metamer, the length of the terminal leaflet and the length of the internode were measured on several days, from the 24 april 2010 to the 19 july 2010.

The metamers which have no leaflets are omitted.
}
\usage{data("rosephytomer")}
\format{
  A data frame with 643 rows (4 plants, 7, 8 or 9 metamers per plant, 37 days of observation) and 6 columns:
  \describe{
    \item{\code{date}}{a POSIXct}
    \item{\code{nplant}}{a factor with levels \code{113} \code{114} \code{118} \code{121}. Numbers of the plants.}
    \item{\code{rank}}{numeric. Rank of the metamer on the stem.}
    \item{\code{lleaflet}, \code{linternode}}{numeric. Length of the terminal leaflet, length of the internode.}
    \item{\code{phytomer}}{factor. Identifiers of the metamers.}
}
}
\source{
Demotes-Mainard, S., Bertheloot, J., Boumaza, R., Huché-Thélier, L., Guéritaine, G., Guérin, V. and Andrieu, B. (2013). Rose bush leaf and internode expansion dynamics: analysis and development of a model capturing interplant variability. Frontiers in Plant Science 4: 418. Doi: 10.3389/fpls.2013.00418
}
\examples{
data(rosephytomer)
as.foldert(rosephytomer, method = 1, ind = "phytomer", timecol = "date", same.rows = TRUE)
}
\keyword{datasets}
