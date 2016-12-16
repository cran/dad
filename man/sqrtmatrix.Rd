\name{sqrtmatrix}
\alias{sqrtmatrix}
\title{
    Square root of a symmetric, positive semi-definite matrix
}
\description{
    Calculation of the square root of a positive semi-definite matrix (see Details for the definition of such a matrix).
}
\usage{
sqrtmatrix(mat)
}
\arguments{
  \item{mat}{
       numeric matrix.
}
}
\details{
    The matrix \code{mat} must be symmetric and positive semi-definite. Otherwise, there is an error.
    
    The square root of the matrix \code{mat} is the positive semi-definite matrix \code{M} such as \code{t(M) \%*\% M = mat}.
    Do not confuse with \code{sqrt(mat)}, which returns the square root of the elements of \code{mat}.

    The computation is based on the diagonalisation of \code{mat}. The eigenvalues smaller than 10^-16 are identified as null values.
}
\value{
    Matrix: the square root of the matrix \code{mat}.                                   
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}
\examples{
    M2 <- matrix(c(5, 4, 4, 5), nrow = 2)
    M <- sqrtmatrix(M2)
    M
}
