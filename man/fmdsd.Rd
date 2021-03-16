\name{fmdsd}
\alias{fmdsd}

\title{
Multidimensional scaling of probability densities
}
\description{
Applies the multidimensional scaling (MDS) method to probability densities in order to describe a data folder, consisting of \eqn{T} groups of individuals on which are observed \eqn{p} variables. It returns an object of class \code{fmdsd}. It applies \code{\link{cmdscale}} to the distance matrix between the \eqn{T} densities.
}
\usage{
fmdsd(xf, group.name = "group", gaussiand = TRUE, distance = c("jeffreys", "hellinger",
    "wasserstein", "l2", "l2norm"), windowh=NULL, data.centered = FALSE,
    data.scaled = FALSE, common.variance = FALSE, add = TRUE, nb.factors = 3,
    nb.values = 10, sub.title = "", plot.eigen = TRUE, plot.score = FALSE, nscore = 1:3,
    filename = NULL)
}
\arguments{
  \item{xf}{
       object of class \code{"\link{folder}"} or data.frame.
       \itemize{
       \item If it is an object of class \code{"folder"}, its elements are data frames with \eqn{p} numeric columns.
       If there are non numeric columns, there is an error.
       The \eqn{t^{th}} element (\eqn{t = 1, \ldots, T}) matches with the \eqn{t^{th}} group.             
       \item If it is a data frame, the column with name given by the \code{group.name} argument is a factor giving the groups.
       The other columns are all numeric; otherwise, there is an error.
       }
}
  \item{group.name}{
       string.
       \itemize{
       \item If \code{xf} is an object of class \code{"\link{folder}"}, it is the name of the grouping variable in the returned results. The default is \code{groupname  = "group"}.
       \item If \code{xf} is a data frame, it is the name of the column of \code{xf} containing the groups.
       }
}
  \item{gaussiand}{
       logical. If \code{TRUE} (default), the probability densities are supposed Gaussian. If \code{FALSE}, densities are estimated using the Gaussian kernel method.
}
  \item{distance}{
       The distance or divergence used to compute the distance matrix between the densities.
       
       If \code{gaussiand = TRUE}, the densities are parametrically estimated and the distance can be:
       \itemize{
       \item \code{"jeffreys"} (default) Jeffreys measure (symmetrised Kullback-Leibler divergence),
       \item \code{"hellinger"} the Hellinger (Matusita) distance,
       \item \code{"wasserstein"} the Wasserstein distance,
       \item \code{"l2"} the \eqn{L^2} distance,
       \item \code{"l2norm"} the densities are normed and the \eqn{L^2} distance between these normed densities is used;
       }
       If \code{gaussiand = FALSE}, the densities are estimated by the Gaussian kernel method and the distance can be
       \code{"l2"} (default) or \code{"l2norm"}.
}
  \item{windowh}{
       either a list of \eqn{T} bandwidths (one per density associated to a group), or a strictly positive number. If \code{windowh = NULL} (default), the bandwidths are automatically computed. See Details.
       
       Omitted when \code{distance} is \code{"hellinger"}, \code{"jeffreys"} or \code{"wasserstein"} (see Details).
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
   \item{add}{
       logical indicating if an additive constant should be computed and added to the non diagonal dissimilarities such that the modified dissimilarities are Euclidean (default \code{TRUE}; see \code{add} argument of \code{\link{cmdscale}}).
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
}
\details{
    In order to compute the distances/dissimilarities between the groups, the \eqn{T} probability densities \eqn{f_t} corresponding to the \eqn{T} groups of individuals are either parametrically estimated (\code{gaussiand = TRUE}) or estimated using the Gaussian kernel method (\code{gaussiand = FALSE}). In the latter case, the \code{windowh} argument provides the list of the bandwidths to be used. Notice that in the multivariate case (\eqn{p}>1), the bandwidths are positive-definite matrices.

    If \code{windowh} is a numerical value, the matrix bandwidth is of the form \eqn{h S}, where \eqn{S} is either the square root of the covariance matrix (\eqn{p}>1) or the standard deviation of the estimated density.  
    
    If \code{windowh = NULL} (default), \eqn{h} in the above formula is computed using the \code{\link{bandwidth.parameter}} function.

    The distance or dissimilarity between the estimated densities is either the \eqn{L^2} distance, the Hellinger distance, Jeffreys measure (symmetrised Kullback-Leibler divergence) or the Wasserstein distance.
    \itemize{
    \item If it is the \code{L^2} distance (\code{distance="l2"} or \code{distance="l2norm"}), the densities can be either parametrically estimated or estimated using the Gaussian kernel.
    \item If it is the Hellinger distance (\code{distance="hellinger"}), Jeffreys measure (\code{distance="jeffreys"}) or the Wasserstein distance (\code{distance="wasserstein"}), the densities are considered Gaussian and necessarily parametrically estimated.
    }
}
\value{
    Returns an object of class \code{fmdsd}, i.e. a list including:
    
    \item{inertia}{data frame of the eigenvalues and percentages of inertia.}
    \item{scores}{data frame of the \code{nb.factors} first principal coordinates.}
    \item{means }{list of the means.}
    \item{variances }{list of the covariance matrices.}
    \item{correlations }{list of the correlation matrices.}
    \item{skewness }{list of the skewness coefficients.}
    \item{kurtosis }{list of the kurtosis coefficients.}
}
\references{
    Boumaza, R., Yousfi, S., Demotes-Mainard, S. (2015). Interpreting the principal component analysis of multivariate density functions. Communications in Statistics - Theory and Methods, 44 (16), 3321-3339.
    
    Delicado, P. (2011). Dimensionality reduction when data are density functions. Computational Statistics & Data Analysis, 55, 401-420.
    
    Yousfi, S., Boumaza, R., Aissani, D., Adjabi, S. (2014). Optimal bandwith matrices in functional principal component analysis of density function. Journal of Statistical Computation and Simulation, 85 (11), 2315-2330.
    
    Cox, T.F., Cox, M.A.A. (2001). Multimensional Scaling, second ed. Chapman & Hall/CRC.
}
\author{
Rachid Boumaza, Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
    \link{fpcad}
    \link{print.fmdsd}, \link{plot.fmdsd}, \link{interpret.fmdsd}, \link{bandwidth.parameter}
}
\examples{
data(roses)
rosesf <- as.folder(roses[,c("Sha","Den","Sym","rose")])

# MDS on Gaussian densities (on sensory data)

# using jeffreys measure (default):
resultjeff <- fmdsd(rosesf, distance = "jeffreys")
print(resultjeff)
plot(resultjeff)

\dontrun{
# Applied to a data frame:
resultjeffdf <- fmdsd(roses[,c("Sha","Den","Sym","rose")],
                      distance = "jeffreys", group.name = "rose")
print(resultjeffdf)
plot(resultjeffdf)
}

# using the Hellinger distance:
resulthellin <- fmdsd(rosesf, distance = "hellinger")
print(resulthellin)
plot(resulthellin)

# using the Wasserstein distance:
resultwass <- fmdsd(rosesf, distance = "wasserstein")
print(resultwass)
plot(resultwass)

# Gaussian case, using the L2-distance:
resultl2 <- fmdsd(rosesf, distance = "l2")
print(resultl2)
plot(resultl2)

# Gaussian case, using the L2-distance between normed densities:
resultl2norm <- fmdsd(rosesf, distance = "l2norm")
print(resultl2norm)
plot(resultl2norm)

\dontrun{
# Non Gaussian case, using the L2-distance,
# the densities are estimated using the Gaussian kernel method:
result <- fmdsd(rosesf, distance = "l2", gaussiand = FALSE, group.name = "rose")
print(result)       
plot(result)
}
}
