\name{matjeffreys}
\alias{matjeffreys}
\title{
    Matrix of Jeffreys measures (symmetrised Kullback-Leibler divergences) between Gaussian densities
}
\description{
    Computes the matrix of the Jeffreys measures between several multivariate (\eqn{p > 1}) or univariate (\eqn{p = 1}) Gaussian densities, given samples.
}
\usage{
matjeffreys(x)
}
\arguments{
  \item{x}{
       object of class "folder" containing the data. Its elements have only numeric variables (observations of the probability densities).
       If there are non numeric variables, there is an error.
}
}
\value{
    Positive symmetric matrix whose order is equal to the number of densities, consisting of the pairwise Jeffreys measures between the Gaussian densities. 
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}
\seealso{
   \code{\link{matjeffreyspar}} if the parameters of the Gaussian densities are known.
}
\examples{
    data(roses)
    
    # Multivariate:
    X <- as.folder(roses[,c("Sha","Den","Sym","rose")], groups = "rose")
    summary(X)
    matjeffreys(X)
    
    # Univariate :
    X1 <- as.folder(roses[,c("Sha","rose")], groups = "rose")
    summary(X1)
    matjeffreys(X1)
}
