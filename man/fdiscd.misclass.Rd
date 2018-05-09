\name{fdiscd.misclass}
\alias{fdiscd.misclass}
\title{
  Misclassification ratio in functional discriminant analysis of probability densities.
}
\description{
  Computes the one-leave-out misclassification ratio of the rule allocating \eqn{T} groups of individuals, one group after another, to one class of groups (among \eqn{K} classes of groups) using the \eqn{L^2} distances between the density function associated to the group to allocate and the density functions associated to the \eqn{K} classes. 
}
\usage{
fdiscd.misclass(xf, class.var, distance = "l2", crit = 1, gaussiand = TRUE, kern = NULL,
                windowh = NULL)
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
       The distance or divergence used to compute the differences between densities.
       It can be:
       \itemize{
       \item \code{"l2"} the \eqn{L^2} distance
       \item \code{"hellinger"} the Hellinger (Matusita) distance
       \item \code{"jeffreys"} the Jeffreys measure (symmetrised Kullback-Leibler divergence)
       }
}
  \item{crit}{
       1, 2 or 3. In order to select the densities associated to the classes. See Details.
       
       If \code{distance} is \code{"hellinger"} or \code{"jeffreys"}, \code{crit} is necessarily \code{1} (see Details).
}
  \item{gaussiand}{
       logical. If \code{TRUE} (default), the probability densities are supposed Gaussian. If \code{FALSE}, densities are estimated using the Gaussian kernel method.
       
       If \code{distance} is \code{"hellinger"} or \code{jeffreys}, \code{gaussiand} is necessarily \code{TRUE}.
}
  \item{kern}{
       string. If \code{gaussiand = FALSE}, this argument sets the kernel used in the estimation method. Currently, only the Gaussian kernel is available: the settings \code{kern = "gauss"} and \code{kern = NULL} are equivalent.
       
       Omitted when \code{distance} is \code{"hellinger"} or \code{jeffreys}.
}                                                                                                        
  \item{windowh}{
       strictly positive numeric value. If \code{windowh = NULL} (default), the bandwidths are computed using the \code{\link{bandwidth.parameter}} function.
       
       Omitted when \code{distance} is \code{"hellinger"} or \code{jeffreys}.
}
}
\details{
 To the group \eqn{t} is associated the density denoted \eqn{f_t}. To the class \eqn{k} consisting of \eqn{T_k} groups is associated the density denoted \eqn{g_k}. The \code{crit} argument selects the estimation method of the \eqn{K} densities \eqn{g_k}.
   
    \enumerate{
    \item
    The density \eqn{g_k} is estimated using the whole data of this class, that is the rows of \code{x} corresponding to the \eqn{T_k} groups of the class \eqn{k}. 
    \item 
    The \eqn{T_k} densities \eqn{f_t} are estimated using the corresponding data from \code{x}. Then they are averaged to obtain an estimation of the density \eqn{g_k}, that is \eqn{g_k = \frac{1}{T_k} \, \sum{f_t}}{g_k = (1/T_k)\sum(f_t)}.
    \item 
    Each previous density \eqn{f_t} is weighted by \eqn{n_t} (the number of rows of \eqn{x} corresponding to \eqn{f_t}). Then they are averaged, that is \eqn{g_k = \frac{1}{\sum n_t} \sum n_t f_t}{g_k = (1/\sum n_t) \sum n_t f_t}.
    }
    The last two methods are available only for the \eqn{L^2}-distance. If the divergences between densities are computed using the Hellinger distance or the Jeffreys measure, only the first of these methods is available.
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
    \item{distances }{matrix with \eqn{T} rows and \eqn{K} columns, of the distances (\eqn{d_{tk}}): \eqn{d_{tk}} is the distance between the group \eqn{t} and the class \eqn{k},}
    \item{proximities }{matrix of the proximity indices (in percents) between the groups and the classes. The proximity of the group \eqn{t} to the class \eqn{k} is computed as so: \eqn{(1/d_{tk})/\sum_{l=1}^{l=K}(1/d_{tl})}.}
}
\references{
    Boumaza, R. (2004). Discriminant analysis with independently repeated multivariate measurements: an \eqn{L^2} approach. Computational Statistics & Data Analysis, 47, 823-843.
    
    Rudrauf, J.M., Boumaza, R. (2001). Contribution à l'étude de l'architecture médiévale: les caractéristiques des pierres à bossage des châteaux forts alsaciens. Centre de Recherches Archéologiques Médiévales de Saverne, 5, 5-38.
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
