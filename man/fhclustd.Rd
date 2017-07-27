\name{fhclustd}
\alias{fhclustd}

\title{
Hierarchic cluster analysis of probability densities
}
\description{
Performs functional hierarchic cluster analysis of probability densities. It returns an object of class \code{\link{hclust}}.
}
\usage{
fhclustd(xf, gaussiand=TRUE, kern = NULL, windowh=NULL, normed=TRUE, centered=FALSE,
         data.centered=FALSE, data.scaled=FALSE,  common.variance = FALSE,
         sub.title="", filename=NULL, method.hclust = "complete", members = NULL)
}
\arguments{
  \item{xf}{
       object of class \code{\link{folder}}. Its elements are numeric data frames with \eqn{p} columns.
       If there are non numeric columns, there is an error.
       The \eqn{t^{th}} element (\eqn{t = 1, \ldots, T}) matches with the \eqn{t^{th}} group.
}       
  \item{gaussiand}{
       logical. If \code{TRUE} (default), the probability densities are supposed Gaussian. If \code{FALSE}, densities are estimated using the Gaussian kernel method.
}
  \item{kern}{
       string. If \code{gaussiand = FALSE} (default is \code{TRUE}), this argument sets the kernel used in the estimation method. Currently, only the Gaussian kernel is available: the settings \code{kern = "gauss"} and \code{kern = NULL} are equivalent.
}                                                                                                        
  \item{windowh}{
       either a list of \eqn{T} bandwidths (one per density associated to a group), or a strictly positive number. If \code{windowh = NULL} (default), the bandwidths are automatically computed. See Details.
}
  \item{normed}{
       logical. If \code{TRUE} (default), the densities are normed before computing the distances; anyway, that does not change the result of the clustering.
}
  \item{centered}{
       logical. If \code{TRUE} (default is \code{FALSE}), the densities are centered.
}
  \item{data.centered}{
       logical. If \code{TRUE} (default is \code{FALSE}), the data of each group are centered.
}
  \item{data.scaled}{
       logical. If \code{TRUE} (default is \code{FALSE}), the data of each group are centered (even if \code{data.centered = FALSE}) and scaled.
}
  \item{common.variance}{
       logical. If \code{TRUE} (default is \code{FALSE}), a common covariance matrix (or correlation matrix if \code{data.scaled = TRUE}), computed on the whole data, is used. If \code{FALSE} (default), a covariance (or correlation) matrix per group is used.
}
  \item{sub.title}{
       string. Subtitle for the graphs (default \code{NULL}).
}
  \item{filename}{
       string. Name of the file in which the results are saved. By default (\code{filename  = NULL}) the results are not saved.
}
  \item{method.hclust}{
       the agglomeration method to be used for the clustering. See the \code{method} argument of the \code{\link{hclust}} function.
}
  \item{members}{
       \code{NULL} or a vector with length size of d. See \code{\link{hclust}}.
}
}
\details{ 
    The distances between the \eqn{T} groups of individuals are given by the \eqn{L^2}-distances between the \eqn{T} probability densities \eqn{f_t} corresponding to these groups. The \code{\link{hclust}} function is then applied to the distance matrix to perform the hierarchical clustering on the \eqn{T} groups.
    
    The probability densities are either parametrically estimated (\code{gaussiand = TRUE}) or estimated using the Gaussian kernel method (\code{gaussiand = FALSE}). In the latter case, the \code{windowh} argument provides the list of the bandwidths to use. Notice that in the multivariate case (\eqn{p}>1) the bandwidths are positive-definite matrices.

    If \code{windowh} is a numerical value, the matrix bandwidth is of the form \eqn{h S}, where \eqn{S} is either the square root of the covariance matrix (\eqn{p}>1) or the standard deviation of the estimated density.  
    
    If \code{windowh = NULL} (default), \eqn{h} in the above formula is computed using the \code{\link{bandwidth.parameter}} function.
}
\value{
    Returns an object of class \code{fhclustd}, that is a list including:
    \item{distances }{matrix of the \eqn{L^2}-distances between the estimated densities.}
    \item{clust }{an object of class \code{\link{hclust}}.}
}
%\references{
%    
%}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
    \link{fdiscd.predict}, \link{fdiscd.misclass}
}
\examples{
data(castles.dated)
xf <- as.folder(castles.dated$stones)
result <- fhclustd(xf)
print(result)
print(result, dist.print = TRUE)
plot(result)
plot(result, hang = -1)

# Use cutree (stats package) to get the partition
cutree(result$clust, k = 1:5)
cutree(result$clust, k = 5)
cutree(result$clust, h = 0.18)
}
