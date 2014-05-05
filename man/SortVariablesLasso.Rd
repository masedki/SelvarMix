\name{SortVariablesLasso}
\alias{SortVariablesLasso}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Variable ranking with LASSO in model-based clustering 
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SortVariablesLasso(data, nbCluster, lambda, rho, supervised = FALSE, knownlabels = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{data}{
  matrix  containing quantitative data. 
  Rows correspond to observations and
  columns correspond to variables.}
  
  \item{nbCluster}{
  numeric listing the number of clusters (must be integers).}
  
  \item{lambda}{
  numeric listing of means penalties.}
  
  
  \item{rho}{
  numeric listing of precision matrix penalties.}

  \item{supervised}{
  TRUE for discriminant analysis (FALSE by default).}

  \item{knownlabels}{
  an integer vector or a factor of size
  number of observations. Each cell corresponds to a
  cluster affectation. So the maximum value is the number
  of clusters.}

}
\value{
matrix with rows corresponding to variables raking. Each 
row corresponds to a value in nbCluster. 
}
\references{
  Zhou, H., Pan, W., and Shen, X., 2009. "Penalized model-based 
  clustering with unconstrained covariance matrices". 
  Electronic Journal of Statistics, vol. 3, pp.1473–1496.
}
\author{
Mohammed Sedki <mohammed.sedki@u-psud.fr>
}

\examples{
## Simulated data  example as shown in Celeux et al. (2014) (correlated scenario 2) 
## n = 2000 observations, p = 14

data(scenarioCor)
data.cor <- scenarioCor[,1:14]
labels.cor <-scenarioCor[,15]

lambda <- seq(0.1,  100, length = 50)
rho <- seq(1, 2, length=2)
nbCluster <-  4

## unsupervised clustering  
var.ranking.clust <- SortVariablesLasso(data.cor, nbCluster, lambda, rho)

 
## discriminant analysis
var.ranking.learn <- SortVariablesLasso(data.cor, nbCluster, lambda, rho, supervised = TRUE, labels.cor)

}
\keyword{Variable ranking}

