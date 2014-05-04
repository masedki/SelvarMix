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

## Simulated data  example as shown in Celeux et al. (2014) (independent scenario 4) 
## n = 300 observations, p = 25

data(scenarioIndep)
lambda <- seq(1, 20, length = 50)
rho <- c(1, 5)
pack.size <- 5
nbCluster <- 3

## unsupervised clustering  
var.ranking.1 <- SortVariablesLasso(data, nbCluster, lambda, rho)

 
## discriminant analysis
var.ranking.2 <- SortVariablesLasso(data, nbCluster, lambda, rho, supervised = TRUE, label)

}
\keyword{Variables raking}

