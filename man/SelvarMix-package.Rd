\docType{package}
\name{SelvarMix-package}
\alias{SelvarMix-package}
\title{REGULARIZATION FOR VARIABLE SELECTION IN MODEL-BASED CLUSTERING}
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
 ## Simulated data  example as shown in Celeux et al. (2014) (independent scenario 4) 
## n = 300 observations, p = 25

data(scenarioIndep)
lambda <- seq(1, 20, length = 50)
rho <- c(1, 5)
pack.size <- 5
nbCluster <- 3
criterion <- c("BIC","ICL")
models <- mixmodGaussianModel(family = "spherical", equal.proportions = TRUE)
modelReg <- c("LI","LB", "LC")
modelIndep <- c("LI","LB")

## unsupervised clustering  
simulate.1 <- SelvarMix(data, nbCluster, lambda, rho, pack.size, criterion, models, modelReg, modelIndep)
 
## discriminant analysis
criterion <- "BIC"
simulate.2 <- SelvarMix(data, nbCluster, lambda, rho, pack.size, criterion, models, modelReg, modelIndep, supervised = TRUE, label)
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
  Statistics and Data Analysis, vol. 53/11, pp. 3872–3882.

  Zhou, H., Pan, W., and Shen, X., 2009. "Penalized model-based 
  clustering with unconstrained covariance matrices". 
  Electronic Journal of Statistics, vol. 3, pp.1473–1496.

}
\keyword{package}

