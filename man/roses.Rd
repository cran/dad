\name{roses}
\alias{roses}
\docType{data}
\title{Roses data     
}
\description{
    Sensory data characterising the visual aspect of 10 rosebushes
}
\usage{data(roses)}
\format{
  \code{roses} is a data frame of sensory data with 420 rows (10 products, 14 assessors, 3 sessions) and 17 columns. The first 16 columns are numeric and correspond to 16 visual characteristics of rosebushes. The last column is a factor giving the name of the corresponding rosebush.
  \itemize{
    \item{\code{Sha: }} {top sided shape}
    \item{\code{Den: }} {foliage thickness}
    \item{\code{Sym: }} {plant symmetry}
    \item{\code{Vgr: }} {stem vigour}
    \item{\code{Qrm: }} {quantity of stems}
    \item{\code{Htr: }} {branching level}
    \item{\code{Qfl: }} {quantity of flowers}
    \item{\code{Efl: }} {staggering of flowering}
    \item{\code{Mvfl: }} {flower enhancement}
    \item{\code{Difl: }} {flower size}
    \item{\code{Qfr: }} {quantity of faded flowers/fruits}
    \item{\code{Qbt: }} {quantity of floral buds}
    \item{\code{Defl: }} {density of flower petals}
    \item{\code{Vcfl: }} {intensity of flower colour}
    \item{\code{Tfe: }} {leaf size}
    \item{\code{Vfe: }} {darkness of leaf colour}
    \item{\code{Lot: }} {factor with 10 levels: \code{A}, \code{B}, \code{C}, \code{D}, 
                         \code{E}, \code{F}, \code{G}, \code{H}, \code{I} and \code{J}}
  }
}
\source{
Boumaza, R., Huché-Thélier, L., Demotes-Mainard, S., Le Coz, E., Leduc, N., Pelleschi-Travier, S., 
Qannari, E.M., Sakr, S., Santagostini, P., Symoneaux, R., Guérin, V. (2010). Sensory profile 
and preference analysis in ornamental horticulture: The case of rosebush. Food Quality and Preference,
21, 987-997. 
}
\examples{
data(roses)
summary(roses)
}

\keyword{datasets}
