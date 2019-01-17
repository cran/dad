\name{mathellinger}
\alias{mathellinger}
\title{
    Matrix of Hellinger distances between Gaussian densities
}
\description{
     Computes the matrix of the Hellinger distances between several multivariate (\eqn{p > 1}) or univariate (\eqn{p = 1}) Gaussian densities  given samples and using \code{\link{hellinger}}.
}
\usage{
mathellinger(x)
}
\arguments{
  \item{x}{
       object of class "folder" containing the data. Its elements have only numeric variables (observations of the probability densities).
       If there are non numeric variables, there is an error.
}
}
\value{
    Positive symmetric matrix whose order is equal to the number of densities, consisting of the pairwise Hellinger distances between the probability densities. 
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}
\seealso{
   \code{\link{hellinger}}.
   
   \code{\link{mathellingerpar}} when the probability densities are Gaussian, given the parameters (means and variances).
}
\examples{
    data(roses)
    
    # Multivariate:
    X <- as.folder(roses[,c("Sha","Den","Sym","rose")], groups = "rose")
    summary(X)
    mathellinger(X)
    
    # Univariate :
    X1 <- as.folder(roses[,c("Sha","rose")], groups = "rose")
    summary(X1)
    mathellinger(X1)
}
