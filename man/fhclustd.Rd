\name{fhclustd}
\alias{fhclustd}

\title{
Hierarchic cluster analysis of probability densities
}
\description{
Performs functional hierarchic cluster analysis of probability densities. It returns an object of class \code{\link{fhclustd}}. It applies \code{\link{hclust}} to the distance matrix between the \eqn{T} densities.
}
\usage{
fhclustd(xf, distance = "l2", gaussiand=TRUE, kern = NULL, windowh=NULL,
			normed=(distance == "l2"), data.centered=FALSE, data.scaled=FALSE,
      common.variance=FALSE, sub.title="", filename=NULL,
      method.hclust = "complete", 
			group.name="group")
}
\arguments{
  \item{xf}{
       object of class \code{\link{folder}}. Its elements are data frames with \eqn{p} numeric columns.
       If there are non numeric columns, there is an error.
       The \eqn{t^{th}} element (\eqn{t = 1, \ldots, T}) matches with the \eqn{t^{th}} group.
}       
  \item{distance}{
       The distance or divergence used to compute the distance matrix between the densities.
       It can be:
       \itemize{
       \item \code{"l2"} the \eqn{L^2} distance
       \item \code{"hellinger"} the Hellinger (Matusita) distance
       \item \code{"jeffreys"} the Jeffreys measure (symmetrised Kullback-Leibler divergence)
       }
}
  \item{gaussiand}{
       logical. If \code{TRUE} (default), the probability densities are supposed Gaussian. If \code{FALSE}, densities are estimated using the Gaussian kernel method.
       
       If \code{distance} is \code{"hellinger"} or \code{jeffreys}, \code{gaussiand} is necessarily \code{TRUE} (see Details).
}
  \item{kern}{
       string. If \code{gaussiand = FALSE} (default is \code{TRUE}), this argument sets the kernel used in the estimation method. Currently, only the Gaussian kernel is available: the settings \code{kern = "gauss"} and \code{kern = NULL} are equivalent.

       Omitted when \code{distance} is \code{"hellinger"} or \code{jeffreys} (see Details).
}                                                                                                        
  \item{windowh}{
       either a list of \eqn{T} bandwidths (one per density associated to a group), or a strictly positive number. If \code{windowh = NULL} (default), the bandwidths are automatically computed. See Details.
       
       Omitted when \code{distance} is \code{"hellinger"} or \code{jeffreys} (see Details).
}
  \item{normed}{
       logical. If \code{TRUE} (default if \code{distance="l2"}), the densities are normed before computing the distances; anyway, that does not change the result of the clustering.
       
       If \code{distance} is \code{"hellinger"} or \code{jeffreys}, \code{normed} is necessarily \code{FALSE} (see Details).
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
       string. If provided, the subtitle for the graphs.
%       string. Subtitle for the graphs (default \code{NULL}).
}
  \item{filename}{
       string. Name of the file in which the results are saved. By default (\code{filename  = NULL}) the results are not saved.
}
  \item{method.hclust}{
       the agglomeration method to be used for the clustering. See the \code{method} argument of the \code{\link{hclust}} function.
}
%  \item{members}{
%       \code{NULL} or a vector with length the number of groups, i.e. the length of \code{xf}. See \code{\link{hclust}}.
%       \code{NULL} or a vector with length size of d. See \code{\link{hclust}}.
%}
  \item{group.name}{
       string. Name of the grouping variable. Default: \code{group.name  = "group"}.
}
}
\details{ 
    In order to compute the distances/dissimilarities between the groups, the \eqn{T} probability densities \eqn{f_t} corresponding to the \eqn{T} groups of individuals are either parametrically estimated (\code{gaussiand = TRUE}) or estimated using the Gaussian kernel method (\code{gaussiand = FALSE}). In the latter case, the \code{windowh} argument provides the list of the bandwidths to be used. Notice that in the multivariate case (\eqn{p}>1), the bandwidths are positive-definite matrices.
    The distances between the \eqn{T} groups of individuals are given by the \eqn{L^2}-distances between the \eqn{T} probability densities \eqn{f_t} corresponding to these groups. The \code{\link{hclust}} function is then applied to the distance matrix to perform the hierarchical clustering on the \eqn{T} groups.
    
    If \code{windowh} is a numerical value, the matrix bandwidth is of the form \eqn{h S}, where \eqn{S} is either the square root of the covariance matrix (\eqn{p}>1) or the standard deviation of the estimated density.  
    
    If \code{windowh = NULL} (default), \eqn{h} in the above formula is computed using the \code{\link{bandwidth.parameter}} function.

    The distance or dissimilarity between the estimated densities is either the \eqn{L^2} distance, the Hellinger distance or the Jeffreys measure (symmetrised Kullback-Leibler divergence).
    \itemize{
    \item If it is the \code{L^2} distance (\code{distance="l2"}), the densities can be either parametrically estimated or estimated using the Gaussian kernel.
    \item If it is the Hellinger distance (\code{distance="hellinger"}) or the Jeffreys measure (\code{distance="jeffreys"}), the densities are considered Gaussian and necessarily parametrically estimated.
    }
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
stones <- castles.dated$stones
periods <- castles.dated$periods

\dontrun{
# L2-distance:

xf <- as.folder(stones)
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

periods123 <- periods[periods$period \%in\% 1:3, "castle"]
stones123 <- stones[stones$castle \%in\% periods123, ]
stones123$castle <- as.factor(as.character(stones123$castle))
yf <- as.folder(stones123)
result123 <- fhclustd(yf)
print(result123)
print(result123, dist.print = TRUE)
plot(result123)
plot(result123, hang = -1)

# Use cutree (stats package) to get the partition
cutree(result123$clust, k = 1:4)
cutree(result123$clust, k = 5)
cutree(result123$clust, h = 0.041)


# Hellinger distance:

resulthel <- fhclustd(yf, distance = "hellinger")
print(resulthel)
print(resulthel, dist.print = TRUE)
plot(resulthel)
plot(resulthel, hang = -1)

# Use cutree (stats package) to get the partition
cutree(resulthel$clust, k = 1:4)
cutree(resulthel$clust, k = 5)
cutree(resulthel$clust, h = 0.041)


# Jeffreys measure:

resultjef <- fhclustd(yf)
print(resultjef)
print(resultjef, dist.print = TRUE)
plot(resultjef)
plot(resultjef, hang = -1)

# Use cutree (stats package) to get the partition
cutree(resultjef$clust, k = 1:4)
cutree(resultjef$clust, k = 5)
cutree(resultjef$clust, h = 0.041)
}
