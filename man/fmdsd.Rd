\name{fmdsd}
\alias{fmdsd}

\title{
Multidimensional scaling of probability densities
}
\description{
Applies the multidimensional scaling (MDS) method to probability densities, in order to describe a three-way data table, consisting of \eqn{T} groups of individuals on which are observed \eqn{p} variables. It returns an object of class \code{fmdsd}. It applies \code{\link{cmdscale}} to the distance matrix between the \eqn{T} densities.
}
\usage{
fmdsd(x, gaussiand = TRUE, windowh = NULL, kern = NULL, normed=FALSE,
    data.centered = FALSE, data.scaled = FALSE, common.variance = FALSE,
    nb.factors = 3, nb.values = 10, sub.title = "", plot.eigen = TRUE,
    plot.score = FALSE, nscore = 1:3, filename = NULL)
}
\arguments{
  \item{x}{
       data frame with (\eqn{p}+1) columns. The first \eqn{p} columns are numeric. The last column is a factor with \eqn{T} levels defining \eqn{T} groups. Each group, say \eqn{t}, consists of \eqn{n_t} individuals. 
}       
  \item{gaussiand}{
       logical. If \code{TRUE} (default), the probability densities are supposed Gaussian. If \code{FALSE}, densities are estimated using the Gaussian kernel method.
}
  \item{kern}{
       string. If \code{gaussiand = FALSE}, this argument sets the kernel used in the estimation method. Currently, only the Gaussian kernel is available: the settings \code{kern = "gauss"} and \code{kern = NULL} are equivalent.
}                                                                                                        
  \item{windowh}{
       either a list of \eqn{T} bandwidths (one per density associated to a group), or a strictly positive number. If \code{windowh=NULL} (default), the bandwidths are automatically computed. See Details.
}
   \item{normed}{
       logical. If \code{TRUE} (default), the densities are normed before computing the distances.
}
  \item{data.centered}{
       logical. If \code{TRUE} (default is \code{FALSE}), the data of each group are centered.
}
  \item{data.scaled}{
       logical. If \code{TRUE} (default is \code{FALSE}), the data of each group are centered (even if \code{data.centered=FALSE}) and scaled.
}
  \item{common.variance}{
       logical. If \code{TRUE} (default is \code{FALSE}), a common covariance matrix (or correlation matrix if \code{data.scaled=TRUE}), computed on the whole data, is used. If \code{FALSE} (default), a covariance (or correlation) matrix per group is used.
}
  \item{nb.factors}{
       numeric. Number of returned principal coordinates (default \code{nb.factors=3}).
       
       Warning: The \code{\link{plot.fmdsd}} and \code{\link{interpret.fmdsd}} functions cannot take into account more than \code{nb.factors}  principal factors.
}
  \item{nb.values}{
       numeric. Number of returned eigenvalues (default \code{nb.values=10}).
}
  \item{sub.title}{
       string. Subtitle for the graphs (default \code{NULL}).
}
  \item{plot.eigen}{
       logical. If \code{TRUE} (default), the barplot of the eigenvalues is plotted.
}
  \item{plot.score}{
       logical.  If \code{TRUE}, the graphs of new coordinates are plotted. A new graphic device is opened for each pair of coordinates defined by \code{nscore} argument.
}
  \item{nscore}{
       numeric vector. If \code{plot.score=TRUE}, the numbers of the principal coordinates which are plotted. By default it is equal to \code{nscore=1:3}. Its components cannot be greater than \code{nb.factors}. 
}
  \item{filename}{
       string. Name of the file in which the results are saved. By default (\code{filename  = NULL}) they are not saved.
}
}
\details{ 
    The \eqn{T} probability densities \eqn{f_t} corresponding to the \eqn{T} groups of individuals are either parametrically estimated (\code{gaussiand=TRUE}) or estimated using the Gaussian kernel method (\code{gaussiand=FALSE}). In the latter case, the \code{windowh} argument provides the list of the bandwidths to be used. Notice that in the multivariate case (\eqn{p}>1), the bandwidths are positive-definite matrices.

    If \code{windowh} is a numerical value, the matrix bandwidth is of the form \eqn{h S}, where \eqn{S} is either the square root of the covariance matrix (\eqn{p}>1) or the standard deviation of the estimated density.  
    
    If \code{windowh = NULL} (default), \eqn{h} in the above formula is computed using the \code{\link{bandwidth.parameter}} function.
}
\value{
    Returns an object of class \code{fmdsd}, i.e. a list including:
    
    \item{inertia}{data frame of the eigenvalues and percentages of inertia.}
    \item{scores}{data frame of the coordinates along the \code{nb.factors} first principal components.}
    \item{norm }{vector of the \eqn{L^2} norms of the densities.}
    \item{means }{list of the means.}
    \item{variances }{list of the covariance matrices.}
    \item{correlations }{list of the correlation matrices.}
    \item{skewness }{list of the skewness coefficients.}
    \item{kurtosis }{list of the kurtosis coefficients.}
}
\references{
    Boumaza, R. (1998). Analyse en composantes principales de distributions gaussiennes multidimensionnelles. Revue de Statistique Appliquee, XLVI (2), 5-20.

    Boumaza, R., Yousfi, S., Demotes-Mainard, S. (2015). Interpreting the principal component analysis of multivariate density functions. Communications in Statistics - Theory and Methods, 44 (16), 3321-3339.
    
    Delicado, P. (2011). Dimensionality reduction when data are density functions. Computational Statistics & Data Analysis, 55, 401-420.
    
    Yousfi, S., Boumaza, R., Aissani, D., Adjabi, S. (2014). Optimal bandwith matrices in functional principal component analysis of density function. Journal of Statistical Computation and Simulation, 85 (11), 2315-2330.
}
\author{
    Rachid Boumaza, Pierre Santagostini, Smail Yousfi, Sabine Demotes-Mainard.
}

\seealso{
    \link{fpcad}
    \link{print.fmdsd}, \link{plot.fmdsd}, \link{interpret.fmdsd}, \link{bandwidth.parameter}
}
\examples{
# MDS on Gaussian densities (on sensory data)
data(roses)
result1 = fmdsd(roses)
print(result1)
plot(result1)

}
