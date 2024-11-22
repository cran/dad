\name{dstatis.inter}
\alias{dstatis.inter}
\alias{dstatis}

\title{
Dual STATIS method (interstructure stage)
}
\description{
Performs the first stage (interstructure) of the dual STATIS method in order to describe a data folder, consisting of \eqn{T} groups of individuals on which are observed \eqn{p} variables. It returns an object of class \code{dstatis}.
}
\usage{
dstatis.inter(xf, normed = TRUE, centered = TRUE, data.scaled = FALSE, nb.factors = 3,
      nb.values = 10, sub.title = "", plot.eigen = TRUE, plot.score = FALSE,
      nscore = 1:3, group.name = "group", filename = NULL)
}
\arguments{
  \item{xf}{
       object of class \code{\link{folder}}. Its elements are data frames with \eqn{p} numeric columns.
       If there are non numeric columns, there is an error.
       The \eqn{t^{th}} element (\eqn{t = 1, \ldots, T}) matches with the \eqn{t^{th}} group.
}       
  \item{normed}{
       logical. If \code{TRUE} (default), the scalar products are normed.
}
  \item{centered}{
       logical. If \code{TRUE} (default), the scalar products are centered.
}
  \item{data.scaled}{
       logical. If \code{TRUE}, the data of each group are centered and scaled. The analysis is then performed on the correlation matrices. If \code{FALSE} (default), the analysis is performed on the covariance matrices.
}
  \item{nb.factors}{
       numeric. Number of returned principal scores (default \code{nb.factors = 3}).
       
%       Warning: The \code{\link{plot.dstatis}} and \code{\link{interpret.dstatis}} functions cannot take into account more than \code{nb.factors}  principal factors.
}
  \item{nb.values}{
       numerical. Number of returned eigenvalues (default \code{nb.values = 10}).
}
  \item{sub.title}{
       string. If provided, the subtitle for the graphs.
%       string. Subtitle for the graphs (default \code{NULL}).
}
  \item{plot.eigen}{
       logical. If \code{TRUE} (default), the barplot of the eigenvalues is plotted.
}
  \item{plot.score}{
       logical.  If \code{TRUE}, the graphs of principal scores are plotted. A new graphic device is opened for each pair of principal scores defined by \code{nscore} argument.
}
  \item{nscore}{
       numeric vector. If \code{plot.score = TRUE}, the numbers of the principal scores which are plotted. By default it is equal to \code{nscore = 1:3}. Its components cannot be greater than \code{nb.factors}. 
}
  \item{group.name}{
       string. Name of the grouping variable. Default: \code{groupname  = "group"}.
}
  \item{filename}{
       string. Name of the file in which the results are saved. By default (\code{filename  = NULL}) the results are not saved.
}
}
\details{ 
    The covariance matrices (if \code{data.scale} is \code{FALSE}) or correlation matrices (if \code{TRUE}) per group are computed. The matrix \eqn{W} of the scalar products between these covariance matrices is then computed.
    
    To perform the STATIS method, see the function DSTATIS%\link[multigroup]{DSTATIS}
    of the \code{multigroup} package.
}
\value{
    Returns an object of class \code{dstatis}, that is a list including:
    \item{inertia }{data frame of the eigenvalues and percentages of inertia.}
    \item{contributions }{data frame of the contributions to the first \code{nb.factors} principal components.}
    \item{qualities }{data frame of the qualities on the first \code{nb.factors} principal factors.}
    \item{scores }{data frame of the first \code{nb.factors} scores of the spectral decomposition of \eqn{W}.}
    \item{norm }{vector of the \eqn{L^2} norms of the densities.}
    \item{means }{list of the means.}
    \item{variances }{list of the covariance matrices.}
    \item{correlations }{list of the correlation matrices.}
    \item{skewness }{list of the skewness coefficients.}
    \item{kurtosis }{list of the kurtosis coefficients.}
}
\references{
    Lavit, C., Escoufier, Y., Sabatier, R., Traissac, P. (1994). The ACT (STATIS method). Computational Statistics & Data Analysis, 18 (1994), 97-119.
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
    \link{print.dstatis}, \link{plot.dstatis}, \link{interpret.dstatis}.
    
    %\link[multigroup]{DSTATIS}
}
\examples{
data(roses)
rosesf <- as.folder(roses[,c("Sha","Den","Sym","rose")])

# Dual STATIS on the covariance matrices
result1 <- dstatis.inter(rosesf, data.scaled = FALSE, group.name = "rose")
print(result1)
plot(result1)

# Dual STATIS on the correlation matrices
result2 <- dstatis.inter(rosesf, data.scaled = FALSE, group.name = "rose")
print(result2)
plot(result2)
}
