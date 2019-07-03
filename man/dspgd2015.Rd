\name{dspgd2015}
\alias{dspgd2015}
\docType{data}
\title{Diploma x Socio professional group by departement in 2015
}
\description{
    Contingency tables of the counts of Diploma x Socio professional group by metroplitan France departement in year 2015.
}
\usage{
data(dspgd2015)
}
\format{
  \code{dspgd2015} is a list of 96 arrays (each one corresponding to a department, designated by its official geographical code) of 4 rows (each one corresponding to a level of diploma) and 6 columns (each one corresponding to a socio professional group). 
  \itemize{
    \item{\code{csp: }} {Socio professional group}
    \item{\code{diplome: }} {Diploma}
    \item{\code{agri: }} {farmer (agriculteur)}
    \item{\code{arti: }} {craftsperson (artisan)}
    \item{\code{cadr: }} {senior manager (cadre sup\'erieur)}
    \item{\code{pint: }} {middle manager (profession interm\'ediaire)}
    \item{\code{empl: }} {employee (employ\'e)}
    \item{\code{ouvr: }} {worker (ouvrier)}
    \item{\code{bepc: }} {brevet}
    \item{\code{cap: }} {NVQ (cap)}
    \item{\code{bac: }} {baccalaureate}
    \item{\code{sup: }} {higher education (sup\'erieur)}
  }
}
\source{
www.insee.fr/fr/statistiques/1893185
Population active de 25 \'a 54 ans ayant un emploi et ch\^omeurs par cat\'egorie socioprofessionnelle et dipl\^ome par commune et d\'epartement (1968 \'a 2015).
}
\author{
Rachid Boumaza, Pierre Santagostini, Smail Yousfi, Sabine Demotes-Mainard
}
\examples{
data(dspgd2015)
names(dspgd2015)
print(dspgd2015[[1]])
}

\keyword{datasets}
