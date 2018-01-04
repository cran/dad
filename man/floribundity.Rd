\name{floribundity}
\alias{floribundity}
\docType{data}
\title{
Rose flowering
}
\description{
These data are collected on eight rosebushes from four varieties, during summer 2010 in Angers, France. They give measures of the flowering.
}
\usage{data("floribundity")}
\format{
  \code{floribundity} is a list of 16 data frames, each corresponding to an observation date. Each one of these data frames has 3 or 4 columns:
  \itemize{
  \item{\code{rose}: the number of the rosebush, that is an identifier.}
  \item{\code{variety}: factor. The variety of the rosebush.}
  \item{\code{area} (when available): numeric. The ratio of flowering area to the whole plant area, measured on the photograph of the rosebush.}
  \item{\code{nflowers} (when available): integer. The number of flowers on the rosebush.}
  }
  The row names of these data frames are the rose identifiers.
}
\examples{
data(floribundity)
foldt <- foldert(floribundity, times = as.Date(names(floribundity)), rows.select = "union")
summary(foldt)
}
\keyword{datasets}
