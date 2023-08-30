\name{foldermtg}
\alias{foldermtg}
\title{
  foldermtg
}
\description{
  An object of S3 class "foldermtg" is built and returned by the function \code{\link{read.mtg}}.
}

%\usage{
%}
%\arguments{
%}

%\details{
%}
\value{
An object of this S3 class is a list of at least 5 data frames (see the Value section in \code{\link{read.mtg}}):
 \code{classes}, \code{description}, \code{features}, \code{topology}, \code{coordinates}...
}

\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
\code{\link{read.mtg}}
\code{\link{print.foldermtg}}
\code{\link{mtgorder}}
}

\references{
    Pradal, C., Godin, C. and Cokelaer, T. (2023). \href{https://mtg.readthedocs.io/en/latest/user/index.html}{MTG user guide}
}

\examples{
mtgfile1 <- system.file("extdata/plant1.mtg", package = "dad")
x1 <- read.mtg(mtgfile1)
print(x1)

mtgfile2 <- system.file("extdata/plant2.mtg", package = "dad")
x2 <- read.mtg(mtgfile2)
print(x2)
}
