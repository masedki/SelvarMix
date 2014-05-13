\docType{package}
\name{SelvarMix-package}
\alias{SelvarMix-package}
\title{REGULARIZATION FOR VARIABLE SELECTION IN MODEL-BASED CLUSTERING AND DISCRIMINANT ANALYSIS}
\description{
  SelvarMix is a package where a regularization approach 
  of variable selection is considered in model-based 
  clustering and discriminant analysis frameworks. 
  First, this procedure consists of ranking the variables 
  with a lasso-like procedure. Second, the method of
  Maugis et al 2009. and Maugis et al 2011. 
  is adapted to define the role of variables in the 
  two frameworks. SelvarMix provides a faster variable 
  selection algorithm than the backward stepwise or forward 
  stepwise algorithms of Maugis et al 2009., allowing us 
  to study high-dimensional datasets.
}
\details{
  \tabular{ll}{ Package: \tab SelvarMix\cr Type: \tab
  Package\cr Version: \tab 1.0\cr Date: \tab
  2014-04-03\cr License: \tab GPL-3 + file LICENSE\cr
  LazyLoad: \tab yes\cr }

  The general purpose of the package is to perform variable 
  selection in model-based clustering and
  discriminant analysis. It focus on model-based 
  clustering, where the clusters are assumed to 
  arise from Gaussian distributions, which 
  provides a rigorous and nice framework to embed 
  the variable selection task in cluster analysis.
  The most achieved model in model-based clustering
  has been presented in Maugis et al 2009. This model, 
  called the \eqn{SRUW} model, is rather complete 
  since it anticipates four different possible roles 
  for the variables: a variable may belongs to \eqn{S} the
  set of relevant variables for the clustering, or 
  it may belong to \eqn{W} the set of independent varables, 
  or it may belong to \eqn{U} the set of irrelevant variables
  depending on a subset \eqn{R} of the relevant variables S 
  according to a linear regression model.  This is the 
  reason why this variable selection model has been 
  christened the \eqn{SRUW} model.
  
  Zhou et al 2009. proposed a different approach via regularization 
  with no retrictive assumption on the component variance matrices. 
  This regularization approach could be expected to be of interest 
  in a high dimensional setting, but it only allows relevant and 
  independent variables. It happens that the stepwises (forward or 
  backward) procedures to estimate the sets \eqn{S, U , R} and \eqn{W} 
  in the \eqn{SRUW} models are painfully slow when numerous variables 
  are available. Thus there is the need for an alternative procedure 
  to get an approximation of these sets in a reasonable CPU time.
 
  SelvarMix proceed in two steps. First, we rank the variables using 
  a procedure analogous to the procedure of Zhou et al 2009. Then the 
  \eqn{SRUW} procedure is run on this ranked set of variables avoiding 
  the exepensive forward or backward procedures involved in the procedure 
  of Maugis et al 2009.
}
\examples{
\dontrun{
## Simulated data  example as shown in Celeux et al. (2014) (correlated scenario 2) 
## n = 2000 observations, p = 14

data(scenarioCor)

lambda <- seq(0.1,  100, length = 50)
rho <- seq(1, 2, length=2)
hybrid.size <- 3
nbCluster <-  c(3,4)
criterion <- "BIC"
models <- mixmodGaussianModel(family = "spherical", equal.proportions = TRUE)
regModel <- c("LI","LB","LC")
indepModel <- c("LI","LB")

## unsupervised clustering  
data.cor <- scenarioCor[,1:14]
simulate.clust  <- SelvarClustLasso(data.cor, nbCluster, lambda, rho, hybrid.size, criterion, models, regModel, indepModel)


## supervised clustering (discriminant analysis)
labels.cor <-scenarioCor[,15]
nbCluster <- 4
simulate.learn <- SelvarLearnLasso(data.cor, nbCluster, lambda, rho, hybrid.size, criterion, models, regModel, indepModel, labels.cor)

}
}
\author{
  Author: Mohammed Sedki and Cathy Maugis-Rabusseau, 
  with contributions from G. Celeux 
  \email{mohammed.sedki@u-psud.fr}
}
\references{
  Maugis, C., Celeux, G., and Martin-Magniette, M. L., 2009. 
  "Variable selection in model-based clustering: 
  A general variable role modeling". Computational 
  Statistics and Data Analysis, vol. 53/11, pp. 3872-3882.

  Maugis, C., Celeux, G., and Martin-Magniette, M. L., 2011. 
  "Variable selection in model-based discriminant analysis". 
  Journal of Multivariate Analysis, vol. 102, pp. 1374-1387.

  Zhou, H., Pan, W., and Shen, X., 2009. "Penalized model-based 
  clustering with unconstrained covariance matrices". 
  Electronic Journal of Statistics, vol. 3, pp.1473-1496.

}
\keyword{package}

