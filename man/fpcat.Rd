\name{fpcat}
\alias{fpcat}

\title{
Functional PCA of probability densities among time
}
\description{
Performs functional principal component analysis of probability densities in order to describe a data ``foldert'', consisting of individuals on which are observed \eqn{p} variables on \eqn{T} times. It returns an object of class \code{fpcat}.
}
\usage{
fpcat(xf, gaussiand = TRUE, kern = NULL, windowh = NULL, normed = TRUE,
    centered = FALSE, data.centered = FALSE, data.scaled = FALSE,
    common.variance = FALSE, nb.factors = 3, nb.values = 10, sub.title = "",
    plot.eigen = TRUE, plot.score = FALSE, nscore = 1:3, group.name="time",
    filename = NULL)
}
\arguments{
  \item{xf}{
       object of class \code{\link{foldert}}. Its elements are data frames with \eqn{p} numeric columns.
       If there are non numeric columns, there is an error.
       The \eqn{t^{th}} element (\eqn{t = 1, \ldots, T}) matches with the \eqn{t^{th}} time of observation.
}
All other arguments are the same as for \code{\link{fpcad}}, except \code{group.name}.
  \item{gaussiand}{
       logical. If \code{TRUE} (default), the probability densities are supposed Gaussian. If \code{FALSE}, densities are estimated using the Gaussian kernel method (as \code{\link{fpcad}}).
}
  \item{kern}{
       string. If \code{gaussiand = FALSE} (default is \code{TRUE}), this argument sets the kernel used in the estimation method. Currently, only the Gaussian kernel is available: the settings \code{kern = "gauss"} and \code{kern = NULL} are equivalent (as \code{\link{fpcad}}).
}                                                                                                        
  \item{windowh}{
       either a list of \eqn{T} bandwidths (one per density associated to a group), or a strictly positive number. If \code{windowh = NULL} (default), the bandwidths are automatically computed (as \code{\link{fpcad}}). See Details.
}
  \item{normed}{
       logical. If \code{TRUE} (default), the densities are normed before computing the distances (as \code{\link{fpcad}}).
}
  \item{centered}{
       logical. If \code{TRUE} (default is \code{FALSE}), the densities are centered (as \code{\link{fpcad}}).
}
  \item{data.centered}{
       logical. If \code{TRUE} (default is \code{FALSE}), the data of each group are centered (as \code{\link{fpcad}}).
}
  \item{data.scaled}{
       logical. If \code{TRUE} (default is \code{FALSE}), the data of each group are centered (even if \code{data.centered = FALSE}) and scaled (as \code{\link{fpcad}}).
}
  \item{common.variance}{
       logical. If \code{TRUE} (default is \code{FALSE}), a common covariance matrix (or correlation matrix if \code{data.scaled = TRUE}), computed on the whole data, is used. If \code{FALSE} (default), a covariance (or correlation) matrix per group is used (as \code{\link{fpcad}}).
}
  \item{nb.factors}{
       numeric. Number of returned principal scores (default \code{nb.factors = 3}) (as \code{\link{fpcad}}).
       
       Warning: The \code{\link{plot.fpcad}} and \code{\link{interpret.fpcad}} functions cannot take into account more than \code{nb.factors}  principal factors (as \code{\link{fpcad}}).
}
  \item{nb.values}{
       numerical. Number of returned eigenvalues (default \code{nb.values = 10}) (as \code{\link{fpcad}}).
}
  \item{sub.title}{
       string. Subtitle for the graphs (default \code{NULL}) (as \code{\link{fpcad}}).
}
  \item{plot.eigen}{
       logical. If \code{TRUE} (default), the barplot of the eigenvalues is plotted (as \code{\link{fpcad}}).
}
  \item{plot.score}{
       logical.  If \code{TRUE}, the graphs of principal scores are plotted. A new graphic device is opened for each pair of principal scores defined by \code{nscore} argument (as \code{\link{fpcad}}).
}
  \item{nscore}{
       numeric vector. If \code{plot.score = TRUE}, the numbers of the principal scores which are plotted. By default it is equal to \code{nscore = 1:3}. Its components cannot be greater than \code{nb.factors} (as \code{\link{fpcad}}). 
}
  \item{group.name}{
       name of the grouping variable, that is the observation times. Default: \code{"time"}.
}
  \item{filename}{
       string. Name of the file in which the results are saved. By default (\code{filename  = NULL}) the results are not saved (as \code{\link{fpcad}}).
}
}
\details{ 
    The \eqn{T} probability densities \eqn{f_t} corresponding to the \eqn{T} times of observation are either parametrically estimated or estimated using the Gaussian kernel method (see \code{\link{fpcad}} for the use of the arguments indicating the method used to estimate these densities).
}
\value{
    Returns an object of class \code{fpcat}, that is a list including:
    \item{times}{vector of the times of observation.}
    \item{inertia }{data frame of the eigenvalues and percentages of inertia.}
    \item{contributions }{data frame of the contributions to the first \code{nb.factors} principal components.}
    \item{qualities }{data frame of the qualities on the first \code{nb.factors} principal factors.}
    \item{scores }{data frame of the first \code{nb.factors} principal scores.}
    \item{norm }{vector of the \eqn{L^2} norms of the densities.}
    \item{means }{list of the means.}
    \item{variances }{list of the covariance matrices.}
    \item{correlations }{list of the correlation matrices.}
    \item{skewness }{list of the skewness coefficients.}
    \item{kurtosis }{list of the kurtosis coefficients.}
}
\references{
    Boumaza, R. (1998). Analyse en composantes principales de distributions gaussiennes multidimensionnelles. Revue de Statistique Appliquée, XLVI (2), 5-20.

    Boumaza, R., Yousfi, S., Demotes-Mainard, S. (2015). Interpreting the principal component analysis of multivariate density functions. Communications in Statistics - Theory and Methods, 44 (16), 3321-3339.
    
    Delicado, P. (2011). Dimensionality reduction when data are density functions. Computational Statistics & Data Analysis, 55, 401-420.
    
    Yousfi, S., Boumaza, R., Aissani, D., Adjabi, S. (2014). Optimal bandwith matrices in functional principal component analysis of density functions. Journal of Statistical Computation and Simulation, 85 (11), 2315-2330.
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
    \link{print.fpcat}, \link{plot.fpcat}, %\link{interpret.fpcat}, 
    \link{bandwidth.parameter}
}
\examples{
# First example: a normed non-centred PCA of Gaussian densities
# (data: stones of castles)
data(castles.dated)
stones <- castles.dated$stones
periods <- castles.dated$periods
period1 <- periods[periods$period == 1, "castle"]
castles1 <- stones[stones$castle \%in\% period1, 1:4]
period2 <- periods[periods$period == 2, "castle"]
castles2 <- stones[stones$castle \%in\% period2, 1:4]
period3 <- periods[periods$period == 3, "castle"]
castles3 <- stones[stones$castle \%in\% period3, 1:4]
period4 <- periods[periods$period == 4, "castle"]
castles4 <- stones[stones$castle \%in\% period4, 1:4]
period5 <- periods[periods$period == 5, "castle"]
castles5 <- stones[stones$castle \%in\% period5, 1:4]
period6 <- periods[periods$period == 6, "castle"]
castles6 <- stones[stones$castle \%in\% period6, 1:4]
castperiod <- foldert(list(castles1, castles2, castles3, castles4, castles5, castles6),
                      times = ordered(1:6))
fpcacastle <- fpcat(castperiod, group.name = "period")
print(fpcacastle)
plot(fpcacastle)

# Second example: the same castles, with their building year
data(castles)
castyear <- foldert(lapply(castles, "[", 1:4))
fpcayear <- fpcat(castyear, group.name = "year")
print(fpcayear)
plot(fpcayear)
}
