\name{departments}
\alias{departments}
\docType{data}
\title{French departments and regions
}
\description{
    Departments and regions of metropolitan France.
}
\usage{data(departments)}
\format{
  \code{departments} is a data frame with 96 rows and 4 columns (factors):
  \itemize{
    \item{\code{coded: }} {departments: numbers}
    \item{\code{named: }} {departments: names}
    \item{\code{coder: }} {regions: ISO code}
    \item{\code{namer: }} {region: names}
  }
}

\source{  
INSEE. \href{https://www.insee.fr/fr/information/3363419}{Code officiel g\'eographique au 1er janvier 2018}.
}

\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Sabine Demotes-Mainard
}

\examples{
data(departments)
print(departments)
}

\keyword{datasets}
