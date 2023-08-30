\name{discdd.predict}
\alias{discdd.predict}
\title{
  Predicting the class of a group of individuals with discriminant analysis of probability distributions.
}
\description{
  Assigns several groups of individuals, one group after another, to the class of groups (among \eqn{K} classes of groups) which achieves the minimum of the distances or divergences between the probability distribution associated to the group to assign and the \eqn{K} probability distributions associated to the \eqn{K} classes.
}
\usage{
discdd.predict(xf, class.var, distance =  c("l1", "l2", "chisqsym", "hellinger",
           "jeffreys", "jensen", "lp"), crit = 1, misclass.ratio = FALSE, p)
}
\arguments{
  \item{xf}{
       object of class \code{\link{folderh}} with two data frames or list of arrays (or tables).
       \itemize{
       \item If it is a \code{folderh}:
         \itemize{
           \item The first data.frame has at least two columns. One column contains the names of the \eqn{T} groups (all the names must be different). An other column is a factor with \eqn{K} levels partitionning the T groups into K classes.
           \item The second one has \eqn{(q+1)} columns. The first \eqn{q} columns are factors (otherwise, they are coerced into factors). The last column is a factor with \eqn{T} levels defining \eqn{T} groups. Each group, say \eqn{t}, consists of \eqn{n_t} individuals.
         }
       \item If it is a list of arrays or tables, the \eqn{t^{th}} element (\eqn{t = 1, \ldots, T}) is the table of the joint distribution (absolute or relative frequencies) of the \eqn{t^{th}} group. These arrays have the same shape:
       
       Each array (or table) \code{xf[[i]]} has:
          \itemize{
          \item the same dimension(s). If \eqn{q = 1} (univariate), \code{dim(xf[[i]])} is an integer. If \eqn{q > 1} (multivariate), \code{dim(xf[[i]])} is an integer vector of length \code{q}.
          \item the same dimension names \code{dimnames(xf[[i]])} (is non \code{NULL}). These dimnames are the names of the variables.
          }
        }
}       
  \item{class.var}{
       string (if \code{xf} is an object of class \code{"folderh"}) or data.frame with two columns (if \code{xf} is a list of arrays).
       \itemize{
       \item If \code{xf} is of class \code{"folder"}, \code{class.var} is the name of the class variable.
       \item If \code{xf} is a list of arrays or a list of tables, \code{class.var} is a data.frame with at least two columns named \code{"group"} and \code{"class"}.
       The \code{"group"} column contains the names of the \eqn{T} groups (all the names must be different). The \code{"class"} column is a factor with \eqn{K} levels partitioning the \eqn{T} groups into \eqn{K} classes.
       }
}
  \item{distance}{
       The distance or dissimilarity used to compute the distance matrix between the densities.
       It can be:
       \itemize{
       \item \code{"l1"} (default) the \eqn{L^p} distance with \eqn{p = 1}
       \item \code{"l2"} the \eqn{L^p} distance with \eqn{p = 2}
       \item \code{"chisqsym"} the symmetric Chi-squared distance
       \item \code{"hellinger"} the Hellinger metric (Matusita distance)
       \item \code{"jeffreys"} Jeffreys distance (symmetrised Kullback-Leibler divergence)
       \item \code{"jensen"} the Jensen-Shannon distance
       \item \code{"lp"} the \eqn{L^p} distance with \eqn{p} given by the argument \code{p} of the function.  
       }
}
  \item{crit}{
       1 or 2. In order to select the densities associated to the classes. See Details.
}
  \item{misclass.ratio}{
       logical (default \code{FALSE}). If \code{TRUE}, the confusion matrix and misclassification ratio are computed on the groups whose prior class is known. In order to compute the misclassification ratio by the one-leave-out method, use the \code{\link{discdd.misclass}} function.}
  \item{p}{
       integer. Optional. When \code{distance = "lp"} (\eqn{L^p} distance with \eqn{p>2}), \code{p} is the parameter of the distance.
}
}
\details{
\itemize{

\item If \code{xf} is an object of class \code{"folderh"} containing the data:
    
    The \eqn{T} probability distributions \eqn{f_t} corresponding to the \eqn{T} groups of individuals are estimated by frequency distributions within each group.

    To the class \eqn{k} consisting of \eqn{T_k} groups is associated the probability distribution \eqn{g_k}.
    The \code{crit} argument selects the estimation method of the \eqn{g_k}'s.
   
    \itemize{
    \item{\code{crit=1}}
    The probability distribution \eqn{g_k} is estimated using the whole data of this class, that is the rows of \code{x} corresponding to the \eqn{T_k} groups of the class \eqn{k}.
    
    The estimation of the \eqn{g_k}'s uses the same method as the estimation of the \eqn{f_t}'s.
    \item{\code{crit=2}}
    The \eqn{T_k} probability distributions \eqn{f_t} are estimated using the corresponding data from \code{xf}. Then they are averaged to obtain an estimation of the density \eqn{g_k}, that is \eqn{g_k = \frac{1}{T_k} \, \sum{f_t}}{g_k = (1/T_k)\sum{f_t}}.
}

\item If \code{xf} is a list of arrays (or list of tables):
    
    The \eqn{t^{th}} array is the joint frequency distribution of the \eqn{t^{th}} group. The frequencies can be absolute or relative.

    To the class \eqn{k} consisting of \eqn{T_k} groups is associated the probability distribution \eqn{g_k}.
    The \code{crit} argument selects the estimation method of the \eqn{g_k}'s.
    
    \itemize{
    \item{\code{crit=1}}
    \eqn{g_k = \frac{1}{\sum n_t} \sum n_t f_t}{g_k = (1/\sum n_t) \sum n_t f_t},
    where \eqn{n_t} is the total of \code{xf[[t]]}.
    
    Notice that when \code{xf[[t]]} contains relative frequencies, its total is 1.
    That is equivalent to \code{crit=2}.
    \item{\code{crit=2}}
    \eqn{g_k = \frac{1}{T_k} \, \sum f_t}{g_k = (1/T_k)\sum f_t}.
    }
}
}
\value{
    Returns an object of class \code{discdd.predict}, that is a list including:
    \item{prediction }{data frame with 3 columns:
      \itemize{
        \item factor giving the group name. The column name is the same as that of the column (\eqn{q+1}) of \code{x},
        \item \code{class.known}: the prior class of the group if it is available, or NA if not,  
        \item \code{class.predict}: the class allocation predicted by the discriminant analysis method. If \code{misclass.ratio = TRUE}, the class allocations are computed for all groups. Otherwise (default), they are computed only for the groups whose class is unknown.
      }
    }
    \item{distances }{matrix with \eqn{T} rows and \eqn{K} columns, of the distances (\eqn{d_{tk}}): \eqn{d_{tk}} is the distance between the group \eqn{t} and the class \eqn{k}, computed with the measure given by argument,}
    \item{proximities }{matrix of the proximities (in percents). The proximity of a group \eqn{t} to the class \eqn{k} is computed as so: \eqn{(1/d_{tk})/\sum_{l=1}^{l=K}(1/d_{tl})}.}
    \item{confusion.mat }{the confusion matrix (if \code{misclass.ratio = TRUE})}
    \item{misclassed }{the misclassification ratio (if \code{misclass.ratio = TRUE})}
}
\references{
    Rudrauf, J.M., Boumaza, R. (2001). Contribution à l'étude de l'architecture médiévale: les caractéristiques des pierres à bossage des châteaux forts alsaciens, Centre de Recherches Archéologiques médiévales de Saverne, 5, 5-38.
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}
\examples{
data(castles.dated)
data(castles.nondated)
stones <- rbind(castles.dated$stones, castles.nondated$stones)
periods <- rbind(castles.dated$periods, castles.nondated$periods)
stones$height <- cut(stones$height, breaks = c(19, 27, 40, 71), include.lowest = TRUE)
stones$width <- cut(stones$width, breaks = c(24, 45, 62, 144), include.lowest = TRUE)
stones$edging <- cut(stones$edging, breaks = c(0, 3, 4, 8), include.lowest = TRUE)
stones$boss <- cut(stones$boss, breaks = c(0, 6, 9, 20), include.lowest = TRUE )

castlesfh <- folderh(periods, "castle", stones)

# Default: dist="l1", crit=1
discdd.predict(castlesfh, "period")

# With the calculation of the confusion matrix and misclassification ratio
discdd.predict(castlesfh, "period", misclass.ratio = TRUE)

# Hellinger distance
discdd.predict(castlesfh, "period", distance = "hellinger")

# crit=2
discdd.predict(castlesfh, "period", crit = 2)
}
