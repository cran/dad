\name{fmdsd}
\alias{fmdsd}

\title{
Multidimensional scaling of probability densities
}
\description{
Applies the multidimensional scaling (MDS) method to probability densities in order to describe a data folder, consisting of \eqn{T} groups of individuals on which are observed \eqn{p} variables. It returns an object of class \code{fmdsd}. It applies \code{\link{cmdscale}} to the distance matrix between the \eqn{T} densities.
}
\usage{
fmdsd(xf, gaussiand = TRUE, distance = "jeffreys", kern = NULL, windowh=NULL,
    normed = FALSE, data.centered = FALSE, data.scaled = FALSE, common.variance = FALSE,
    nb.factors = 3, nb.values = 10, sub.title = "", plot.eigen = TRUE,
    plot.score = FALSE, nscore = 1:3, filename = NULL, group.name = "group")
}
\arguments{
  \item{xf}{
       object of class \code{\link{folder}}. Its elements are data frames with \eqn{p} numeric columns.
       If there are non numeric columns, there is an error.
       The \eqn{t^{th}} element (\eqn{t = 1, \ldots, T}) matches with the \eqn{t^{th}} group.             
       
       Notice that for the versions earlier than 2.0, fmdsd applied to a data frame.
}
  \item{distance}{
       The distance or divergence used to compute the distance matrix between the densities.
       It can be:
       \itemize{
       \item \code{"jeffreys"} (default) the Jeffreys measure (symmetrised Kullback-Leibler divergence)
       \item \code{"hellinger"} the Hellinger (Matusita) distance
       \item \code{"wasserstein"} the Wasserstein distance
       \item \code{"l2"} the \eqn{L^2} distance
       }
}
  \item{gaussiand}{
       logical. If \code{TRUE} (default), the probability densities are supposed Gaussian. If \code{FALSE}, densities are estimated using the Gaussian kernel method.
       
       If \code{distance} is \code{"hellinger"}, \code{"jeffreys"} or \code{"wasserstein"}, \code{gaussiand} is necessarily \code{TRUE} (see Details).
}
  \item{kern}{
       string. If \code{gaussiand = FALSE} (default is \code{TRUE}), this argument sets the kernel used in the estimation method. Currently, only the Gaussian kernel is available: the settings \code{kern = "gauss"} and \code{kern = NULL} are equivalent.
       
       Omitted when \code{distance} is \code{"hellinger"}, \code{"jeffreys"} or \code{"wasserstein"} (see Details).
}                                                                  
  \item{windowh}{
       either a list of \eqn{T} bandwidths (one per density associated to a group), or a strictly positive number. If \code{windowh = NULL} (default), the bandwidths are automatically computed. See Details.
       
       Omitted when \code{distance} is \code{"hellinger"}, \code{"jeffreys"} or \code{"wasserstein"} (see Details).
}
   \item{normed}{
       logical. If \code{TRUE} (default is \code{FALSE}), the densities are normed before computing the distances.
       
       If \code{distance} is \code{"hellinger"}, \code{"jeffreys"} or \code{"wasserstein"}, \code{normed} is necessarily \code{FALSE} (see Details).
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
  \item{nb.factors}{
       numeric. Number of returned principal coordinates (default \code{nb.factors = 3}).
       
       Warning: The \code{\link{plot.fmdsd}} and \code{\link{interpret.fmdsd}} functions cannot take into account more than \code{nb.factors}  principal factors.
}
  \item{nb.values}{
       numeric. Number of returned eigenvalues (default \code{nb.values = 10}).
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
       numeric vector. If \code{plot.score = TRUE}, the numbers of the principal coordinates which are plotted. By default it is equal to \code{nscore = 1:3}. Its components cannot be greater than \code{nb.factors}. 
}
  \item{filename}{
       string. Name of the file in which the results are saved. By default (\code{filename  = NULL}) they are not saved.
}
  \item{group.name}{
       string. Name of the grouping variable. Default: \code{groupname  = "group"}.
}
}
\details{
    In order to compute the distances/dissimilarities between the groups, the \eqn{T} probability densities \eqn{f_t} corresponding to the \eqn{T} groups of individuals are either parametrically estimated (\code{gaussiand = TRUE}) or estimated using the Gaussian kernel method (\code{gaussiand = FALSE}). In the latter case, the \code{windowh} argument provides the list of the bandwidths to be used. Notice that in the multivariate case (\eqn{p}>1), the bandwidths are positive-definite matrices.

    If \code{windowh} is a numerical value, the matrix bandwidth is of the form \eqn{h S}, where \eqn{S} is either the square root of the covariance matrix (\eqn{p}>1) or the standard deviation of the estimated density.  
    
    If \code{windowh = NULL} (default), \eqn{h} in the above formula is computed using the \code{\link{bandwidth.parameter}} function.

    The distance or dissimilarity between the estimated densities is either the \eqn{L^2} distance, the Hellinger distance, the Jeffreys measure (symmetrised Kullback-Leibler divergence) or the Wasserstein distance.
    \itemize{
    \item If it is the \code{L^2} distance (\code{distance="l2"}), the densities can be either parametrically estimated or estimated using the Gaussian kernel.
    \item If it is the Hellinger distance (\code{distance="hellinger"}), the Jeffreys measure (\code{distance="jeffreys"}) or the Wasserstein distance (\code{distance="wasserstein"}), the densities are considered Gaussian and necessarily parametrically estimated.
    }
}
\value{
    Returns an object of class \code{fmdsd}, i.e. a list including:
    
    \item{inertia}{data frame of the eigenvalues and percentages of inertia.}
    \item{scores}{data frame of the coordinates along the \code{nb.factors} first principal components.}
    \item{norm }{vector of the \eqn{L^2} norms of the densities (only if the distance used is the \eqn{L^2}-distance: \code{distance="l2"}).}
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
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
    \link{fpcad}
    \link{print.fmdsd}, \link{plot.fmdsd}, \link{interpret.fmdsd}, \link{bandwidth.parameter}
}
\examples{
data(roses)
rosesfold <- as.folder(roses)

# MDS on Gaussian densities (on sensory data)

# using the Jeffreys measure (default):
resultjeff <- fmdsd(rosesfold, distance = "jeffreys")
print(resultjeff)
plot(resultjeff)

# using the Hellinger distance:
resulthellin <- fmdsd(rosesfold, distance = "hellinger")
print(resulthellin)
plot(resulthellin)

# using the Wasserstein distance:
resultwass <- fmdsd(rosesfold, distance = "wasserstein")
print(resultwass)
plot(resultwass)

# using the L2-distance:
resultl2 <- fmdsd(rosesfold, distance = "l2")
print(resultl2)
plot(resultl2)
}
