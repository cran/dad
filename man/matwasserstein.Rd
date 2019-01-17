\name{matwasserstein}
\alias{matwasserstein}
\title{
    Matrix of 2-Wassterstein distance between Gaussian densities
}
\description{
    Computes the matrix of the 2-Wassterstein distances between several multivariate (\eqn{p > 1}) or univariate (\eqn{p = 1}) Gaussian densities, given samples.
}
\usage{
matwasserstein(x)
}
\arguments{
  \item{x}{
       object of class "folder" containing the data. Its elements have only numeric variables (observations of the probability densities).
       If there are non numeric variables, there is an error.
}
}
\value{
    Positive symmetric matrix whose order is equal to the number of densities, consisting of the pairwise 2-Wassterstein distance between the Gaussian densities. 
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}
\seealso{
   \code{\link{matwassersteinpar}} if the parameters of the Gaussian densities are known.

}
\examples{
    data(roses)
    
    # Multivariate:
    X <- as.folder(roses[,c("Sha","Den","Sym","rose")], groups = "rose")
    summary(X)
    matwasserstein(X)
    
    # Univariate :
    X1 <- as.folder(roses[,c("Sha","rose")], groups = "rose")
    summary(X1)
    matwasserstein(X1)
}
