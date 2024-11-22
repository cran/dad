\name{dspg}
\alias{dspg}
\docType{data}
\title{Diploma x Socio professional group
}
\description{
    Contingency tables of the counts of Diploma x Socio professional group of France
}
\usage{data(dspg)}
\format{
  \code{dspg} is a list of 7 arrays (each one corresponding to a year: 1968, 1975, 1982, 1990, 1999, 2010, 2015) of 4 rows (each one corresponding to a level of diploma) and 6 columns (each one corresponding to a socio professional group). 
  \describe{
    \item{\code{csp: }}{Socio professional group}
    \item{\code{diplome: }}{Diploma}
    \item{\code{agri: }}{farmer (agriculteur)}
    \item{\code{arti: }}{craftsperson (artisan)}
    \item{\code{cadr: }}{senior manager (cadre sup\'erieur)}
    \item{\code{pint: }}{middle manager (profession interm\'ediaire)}
    \item{\code{empl: }}{employee (employ\'e)}
    \item{\code{ouvr: }}{worker (ouvrier)}
    \item{\code{bepc: }}{brevet}
    \item{\code{cap: }}{NVQ (cap)}
    \item{\code{bac: }}{baccalaureate}
    \item{\code{sup: }}{higher education (sup\'erieur)}
  }
}

\source{
INSEE. \href{https://www.insee.fr/fr/statistiques/1893185}{
Population active de 25 à 54 ans ayant un emploi et chômeurs par catégorie socioprofessionnelle et diplôme par commune et département (1968 à 2015)}.
}

\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Sabine Demotes-Mainard
}

\examples{
data(dspg)
names(dspg)
print(dspg[[1]])
}

\keyword{datasets}
