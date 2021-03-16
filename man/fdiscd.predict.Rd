\name{fdiscd.predict}
\alias{fdiscd.predict}
\title{
  Predicting the class of a group of individuals with discriminant analysis of probability densities.
}
\description{
  Assigns several groups of individuals, one group after another, to the class of groups (among \eqn{K} classes of groups) which achieves the minimum of the distances or divergences between the density function associated to the group to assign and the \eqn{K} density functions associated to the \eqn{K} classes.
}
\usage{
fdiscd.predict(xf, class.var, gaussiand = TRUE,
           distance =  c("jeffreys", "hellinger", "wasserstein", "l2", "l2norm"),
           crit = 1, windowh = NULL, misclass.ratio = FALSE)
}
\arguments{
  \item{xf}{
       object of class \code{\link{folderh}} with two data frames:
       \itemize{
         \item The first one has at least two columns. One column contains the names of the \eqn{T} groups (all the names must be different). An other column is a factor with \eqn{K} levels partitionning the T groups into K classes..
         \item The second one has \eqn{(p+1)} columns. The first \eqn{p} columns are numeric (otherwise, there is an error). The last column is a factor with \eqn{T} levels defining \eqn{T} groups. Each group, say \eqn{t}, consists of \eqn{n_t} individuals.
       }
       
       Notice that for the versions earlier than 2.0, fdiscd.predict applied to two data frames.
}       
  \item{class.var}{
       string. The name of the class variable.
}
  \item{distance}{
       The distance or divergence used to compute the distance matrix between the densities.
       It can be:
       \itemize{
       \item \code{"jeffreys"} (default) Jeffreys measure (symmetrised Kullback-Leibler divergence),
       \item \code{"hellinger"} the Hellinger (Matusita) distance,
       \item \code{"wasserstein"} the Wasserstein distance,
       \item \code{"l2"} the \eqn{L^2} distance,
       \item \code{"l2norm"} the densities are normed and the \eqn{L^2} distance between these normed densities is used;
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
       strictly positive number. If \code{windowh = NULL} (default), the bandwidths are computed using the \code{\link{bandwidth.parameter}} function.
       
       Omitted when \code{distance} is \code{"hellinger"}, \code{"jeffreys"} or \code{"wasserstein"} (see Details).
}
  \item{misclass.ratio}{
       logical (default \code{FALSE}). If \code{TRUE}, the confusion matrix and misclassification ratio are computed on the groups whose prior class is known. In order to compute the misclassification ratio by the one-leave-out method, use the \code{\link{fdiscd.misclass}} function.}
}
\details{
 To the group \eqn{t} is associated the density denoted \eqn{f_t}. To the class \eqn{k} consisting of \eqn{T_k} groups is associated the density denoted \eqn{g_k}. The \code{crit} argument selects the estimation method of the \eqn{K} densities \eqn{g_k}.
%   
    \enumerate{
    \item
    The density \eqn{g_k} is estimated using the whole data of this class, that is the rows of \code{x} corresponding to the \eqn{T_k} groups of the class \eqn{k}. 
    \item 
    The \eqn{T_k} densities \eqn{f_t} are estimated using the corresponding data from \code{x}. Then they are averaged to obtain an estimation of the density \eqn{g_k}, that is \eqn{g_k = (1/T_k)\sum{f_t}}.
    \item 
    Each previous density \eqn{f_t} is weighted by \eqn{n_t} (the number of rows of \eqn{x} corresponding to \eqn{f_t}). Then they are averaged, that is \eqn{g_k = (1/\sum n_t) \sum n_t f_t}.
    }
    The last two methods are available only for the \eqn{L^2}-distance. If the divergences between densities are computed using the Hellinger or Wasserstein distance or Jeffreys measure, only the first of these methods is available.
}
\value{
    Returns an object of class \code{fdiscd.predict}, that is a list including:
    \item{prediction }{data frame with 3 columns:
      \itemize{
        \item factor giving the group name. The column name is the same as that of the column (\eqn{p+1}) of \code{x},
        \item \code{class.known}: the prior class of the group if it is available, or NA if not,  
        \item \code{class.predict}: the class allocation predicted by the discriminant analysis method. If \code{misclass.ratio = TRUE}, the class allocations are computed for all groups. Otherwise (default), they are computed only for the groups whose class is unknown.
      }
    }
    \item{distances }{matrix with \eqn{T} rows and \eqn{K} columns, of the distances (\eqn{d_{tk}}): \eqn{d_{tk}} is the distance between the group \eqn{t} and the class \eqn{k}, computed with the measure given by argument \code{distance} (\eqn{L^2}-distance, Hellinger distance or jeffreys measure),}
    \item{proximities }{matrix of the proximities (in percents). The proximity of a group \eqn{t} to the class \eqn{k} is computed as so: \eqn{(1/d_{tk})/\sum_{l=1}^{l=K}(1/d_{tl})}.}
    \item{confusion.mat }{the confusion matrix (if \code{misclass.ratio = TRUE})}
    \item{misclassed }{the misclassification ratio (if \code{misclass.ratio = TRUE})}
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
data(castles.nondated)
castles.stones <- rbind(castles.dated$stones, castles.nondated$stones)
castles.periods <- rbind(castles.dated$periods, castles.nondated$periods)
castlesfh <- folderh(castles.periods, "castle", castles.stones)

# With the L^2-distance

# - crit=1
resultl2.1 <- fdiscd.predict(castlesfh, "period", distance="l2", crit=1)
print(resultl2.1)

# - crit=2
\dontrun{
resultl2.2 <- fdiscd.predict(castlesfh, "period", distance="l2", crit=2)
print(resultl2.2)
}

# - crit=3
resultl2.3 <- fdiscd.predict(castlesfh, "period", distance="l2", crit=3)
print(resultl2.3)

# With the Hellinger distance
resulthelling <- fdiscd.predict(castlesfh, "period", distance="hellinger")
print(resulthelling)

# With jeffreys measure
resultjeff <- fdiscd.predict(castlesfh, "period", distance="jeffreys")
print(resultjeff)
}
