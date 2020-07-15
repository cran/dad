\name{fdiscd.misclass}
\alias{fdiscd.misclass}
\title{
  Misclassification ratio in functional discriminant analysis of probability densities.
}
\description{
  Computes the one-leave-out misclassification ratio of the rule assigning \eqn{T} groups of individuals, one group after another, to the class of groups (among \eqn{K} classes of groups) which achieves the minimum of the distances or divergences between the density function associated to the group to assign and the \eqn{K} density functions associated to the \eqn{K} classes. 
}
\usage{
fdiscd.misclass(xf, class.var, gaussiand = TRUE,
           distance =  c("jeffreys", "hellinger", "wasserstein", "l2", "l2norm"),
           crit = 1, windowh = NULL)
}
\arguments{
  \item{xf}{
       object of class \code{\link{folderh}} with two data frames:
       \itemize{
         \item The first one has at least two columns. One column contains the names of the \eqn{T} groups (all the names must be different). An other column is a factor with \eqn{K} levels partitionning the T groups into K classes.
         \item The second one has \eqn{(p+1)} columns. The first \eqn{p} columns are numeric (otherwise, there is an error). The last column is a factor with \eqn{T} levels defining \eqn{T} groups. Each group, say \eqn{t}, consists of \eqn{n_t} individuals.
       }
}       
  \item{class.var}{
       string. The name of the class variable.
}
  \item{distance}{
       The distance or dissimilarity used to compute the distance matrix between the densities.
       It can be:
       \itemize{
       \item \code{"jeffreys"} (default) the Jeffreys measure (symmetrised Kullback-Leibler divergence),
       \item \code{"hellinger"} the Hellinger (Matusita) distance,
       \item \code{"wasserstein"} the Wasserstein distance,
       \item \code{"l2"} the \eqn{L^2} distance,
       \item \code{"l2norm"} (only available when crit = 1) the densities are normed and the \eqn{L^2} distance between these normed densities is used;
       }
%       If \code{gaussiand = FALSE}, the densities are estimated by the Gaussian kernel method and the distance can be
%       \code{"l2"} (default) or \code{"l2norm"}.
       If \code{gaussiand = FALSE}, the densities are estimated by the Gaussian kernel method and the distance is \code{"l2"} or \code{"l2norm"}.
}
  \item{crit}{
       1, 2 or 3. In order to select the densities associated to the classes. See Details.
       
       If \code{distance} is \code{"hellinger"}, \code{"jeffreys"} or \code{"wasserstein"}, \code{crit} is necessarily \code{1} (see Details).
}
  \item{gaussiand}{
       logical. If \code{TRUE} (default), the probability densities are supposed Gaussian. If \code{FALSE}, densities are estimated using the Gaussian kernel method.
       
       If \code{distance} is \code{"hellinger"}, \code{"jeffreys"} or \code{"wasserstein"}, \code{gaussiand} is necessarily \code{TRUE}.
}
  \item{windowh}{
       strictly positive numeric value. If \code{windowh = NULL} (default), the bandwidths are computed using the \code{\link{bandwidth.parameter}} function.
       
       Omitted when \code{distance} is \code{"hellinger"}, \code{"jeffreys"} or \code{"wasserstein"} (see Details).
}
}
\details{
    The \eqn{T} probability densities \eqn{f_t} corresponding to the \eqn{T} groups of individuals are either parametrically estimated (\code{gaussiand = TRUE}) or estimated using the Gaussian kernel method (\code{gaussiand = FALSE}). In the latter case, the \code{windowh} argument provides the list of the bandwidths to be used. Notice that in the multivariate case (\eqn{p}>1), the bandwidths are positive-definite matrices.

    The argument \code{windowh} is a numerical value, the matrix bandwidth is of the form \eqn{h S}, where \eqn{S} is either the square root of the covariance matrix (\eqn{p}>1) or the standard deviation of the estimated density.  
    
    If \code{windowh = NULL} (default), \eqn{h} in the above formula is computed using the \code{\link{bandwidth.parameter}} function.

    %So, to the group \eqn{t} is associated the density denoted \eqn{f_t}. 
    
    To the class \eqn{k} consisting of \eqn{T_k} groups is associated the density denoted \eqn{g_k}. The \code{crit} argument selects the estimation method of the \eqn{K} densities \eqn{g_k}.
   
    \enumerate{
    \item
    The density \eqn{g_k} is estimated using the whole data of this class, that is the rows of \code{x} corresponding to the \eqn{T_k} groups of the class \eqn{k}.
    
    The estimation of the densities \eqn{g_k} uses the same method as the estimation of the \eqn{f_t}.
    \item 
    The \eqn{T_k} densities \eqn{f_t} are estimated using the corresponding data from \code{x}. Then they are averaged to obtain an estimation of the density \eqn{g_k}, that is \eqn{g_k = \frac{1}{T_k} \, \sum{f_t}}{g_k = (1/T_k)\sum(f_t)}.
    \item 
    Each previous density \eqn{f_t} is weighted by \eqn{n_t} (the number of rows of \eqn{x} corresponding to \eqn{f_t}). Then they are averaged, that is \eqn{g_k = \frac{1}{\sum n_t} \sum n_t f_t}{g_k = (1/\sum n_t) \sum n_t f_t}.
    }
    The last two methods are only available for the \eqn{L^2}-distance. If the divergences between densities are computed using the Hellinger or Wasserstein distance or the Jeffreys measure, only the first of these methods is available.
    
    The distance or dissimilarity between the estimated densities is either the \eqn{L^2} distance, the Hellinger distance, the Jeffreys measure (symmetrised Kullback-Leibler divergence) or the Wasserstein distance.
    \itemize{
    \item If it is the \code{L^2} distance (\code{distance="l2"} or \code{distance="l2norm"}), the densities can be either parametrically estimated or estimated using the Gaussian kernel.
    \item If it is the Hellinger distance (\code{distance="hellinger"}), the Jeffreys measure (\code{distance="jeffreys"}) or the Wasserstein distance (\code{distance="wasserstein"}), the densities are considered Gaussian and necessarily parametrically estimated.
    }
}
\value{ 
    Returns an object of class \code{fdiscd.misclass}, that is a list including:
    \item{classification }{data frame with 4 columns:
      \itemize{
        \item factor giving the group name. The column name is the same as that of the column (\eqn{p+1}) of \code{x},
        \item the prior class of the group if it is available, or NA if not,
        \item \code{alloc}: the class allocation computed by the discriminant analysis method,
        \item \code{misclassed}: boolean. \code{TRUE} if the group is misclassed, \code{FALSE} if it is well-classed, \code{NA} if the prior class of the group is unknown.
      }
    }
    \item{confusion.mat }{confusion matrix,}
    \item{misalloc.per.class }{the misclassification ratio per class,}
    \item{misclassed }{the misclassification ratio,}
    \item{distances }{matrix with \eqn{T} rows and \eqn{K} columns, of the distances (\eqn{d_{tk}}): \eqn{d_{tk}} is the distance between the group \eqn{t} and the class \eqn{k}, computed with the measure given by argument \code{distance} (\eqn{L^2}-distance, Hellinger distance or Jeffreys measure),}
    \item{proximities }{matrix of the proximity indices (in percents) between the groups and the classes. The proximity of the group \eqn{t} to the class \eqn{k} is computed as so: \eqn{(1/d_{tk})/\sum_{l=1}^{l=K}(1/d_{tl})}.}
}
\references{
    Boumaza, R. (2004). Discriminant analysis with independently repeated multivariate measurements: an \eqn{L^2} approach. Computational Statistics & Data Analysis, 47, 823-843.
    
    Rudrauf, J.M., Boumaza, R. (2001). Contribution ? l'?tude de l'architecture m?di?vale: les caract?ristiques des pierres ? bossage des ch?teaux forts alsaciens. Centre de Recherches Arch?ologiques M?di?vales de Saverne, 5, 5-38.
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}
\examples{
data(castles.dated)
castles.stones <- castles.dated$stones
castles.periods <- castles.dated$periods
castlesfh <- folderh(castles.periods, "castle", castles.stones)
result <- fdiscd.misclass(castlesfh, "period")
print(result)
}
