\name{dad-package}
\alias{dad-package}
\alias{dad}
\docType{package}
\title{
Three-Way Data Analysis Through Densities}

\description{
The three-way data consists of a set of variables measured on several groups of 
individuals. To each group is associated an estimated probability density 
function. The package provides functional methods (principal component 
analysis, multidimensional scaling, cluster analysis, 
discriminant analysis...) for such probability densities.  
}

\details{
\tabular{ll}{
Package: \tab dad\cr
Type: \tab Package\cr
Version: \tab 3.0.1\cr
Date: \tab 2017-09-01\cr
License: \tab GPL-2\cr
}
To cite \code{dad}, use \code{citation("dad")}.

The main functions applying to the probability densities are: \itemize{
\item{\code{\link{fpcad}}: functional principal component analysis,}
\item{\code{\link{fmdsd}}: multidimensional scaling,}
\item{\code{\link{fhclustd}}: hierarchical clustering,}
\item{\code{\link{fdiscd.misclass}}: functional discriminant analysis in order to compute the misclassification ratio with the one-leave-out method,}
\item{\code{\link{fdiscd.predict}}: discriminant analysis in order to predict the class (synonymous with cluster, not to be confused with the class attribute of an R object) of each probability density whose class is unknown.}
}
The above functions are completed by: \itemize{
\item{A \code{print()} method for objects of class \code{fpcad}, \code{fmdsd}, \code{fdiscd.misclass} or \code{fdiscd.predict}, in order to display the results of the corresponding function,}
\item{A \code{plot()} method for objects of class \code{fpcad}, \code{fmdsd} or \code{fhclustd}, in order to display some useful graphics attached to the corresponding function,}
\item{A generic function \code{\link{interpret}} that applies to objects of class \code{fpcad} or \code{fmdsd}, helps the user to interpret the scores returned by the corresponding function, in terms of moments.}
}

We also introduce classes of objects and tools in order to handle collections of data frames: \itemize{
\item{\code{\link{folder}} creates an object of class \code{folder}, that is a list of data frames which have in common the same columns.

The following functions apply to a folder and compute some statistics on the columns of its elements: \code{\link{mean.folder}}, \code{\link{var.folder}}, \code{\link{cor.folder}}, \code{\link{skewness.folder}} or \code{\link{kurtosis.folder}}.}
\item{\code{\link{folderh}} creates an object of class \code{folderh}, that is a list of data frames with a hierarchic relation between each pair of consecutive data frames.}
\item{\code{\link{read.mtg}}} creates an object of class \code{foldermtg} from an MTG (Multiscale Tree Graph) file containing plant architecture data.}
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Sabine Demotes-Mainard with the contributions from Gilles Hunault, Julie Bourbeillon and Besnik Pumo
}
\references{
    Boumaza, R. (1998). Analyse en composantes principales de distributions gaussiennes multidimensionnelles. Revue de Statistique Appliquée, XLVI (2), 5-20.

    Boumaza, R., Yousfi, S., Demotes-Mainard, S. (2015). Interpreting the principal component analysis of multivariate density functions. Communications in Statistics - Theory and Methods, 44 (16), 3321-3339.
    
    Boumaza, R. (2004). Discriminant analysis with independently repeated multivariate measurements: an \eqn{L^2} approach. Computational Statistics & Data Analysis, 47, 823-843.

    Cokelaer, T. and Pradal, C. (2010). \href{http://openalea.gforge.inria.fr/doc/vplants/newmtg/doc/_build/html/user/syntax.html}{MTG user guide}
    
    Delicado, P. (2011). Dimensionality reduction when data are density functions. Computational Statistics & Data Analysis, 55, 401-420.
    
    Rudrauf, J.M., Boumaza, R. (2001). Contribution à l'étude de l'architecture médiévale: les caractéristiques des pierres à bossage des châteaux forts alsaciens. Centre de Recherches Archéologiques Médiévales de Saverne, 5, 5-38.

    Yousfi, S., Boumaza, R., Aissani, D., Adjabi, S. (2014). Optimal bandwith matrices in functional principal component analysis of density function. Journal of Statistical Computation and Simulation, 85 (11), 2315-2330.   
}
