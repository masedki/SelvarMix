\name{SelvarClustLasso}
\alias{SelvarClustLasso}
\title{
Regularization for variable selection in model-based clustering
}
\description{
These function implement the variable selection in model-based clustering 
using a lasso ranking on the variables as described in Celeux et al (2014).
The variable ranking step uses the penalized EM algorithm of Zhou et al (2009).
}
\usage{
SelvarClustLasso(data, 
          nbCluster, 
          lambda, 
          rho, 
          hybrid.size = 3,  
          criterion = "BIC", 
          models = mixmodGaussianModel(listModels = "Gaussian_pk_Lk_C"), 
          regModel = c("LI", "LB", "LC"), 
          indepModel = c("LI", "LB"))
}

\arguments{

  \item{data}{
  matrix  containing quantitative data. 
  Rows correspond to observations and
  columns correspond to variables.
}
  \item{nbCluster}{
 numeric listing the number of clusters (must be integers).
}
  \item{lambda}{
  numeric listing of means penalties.
}
  \item{rho}{
  numeric listing of precision matrix penalties.
}
\item{hybrid.size}{
optionnal parameter make less strength  the hybrid forward and backward 
algorithms to select \eqn{S} and \eqn{W} sets.
}

\item{criterion}{list of character defining the criterion
  to select the best model. The best model is the one with
  the lowest criterion value. Possible values: "BIC",
  "ICL", c("BIC", "ICL"). Default is "BIC".}

\item{models}{a  Rmixmod [\code{\linkS4class{Model}}] object
  defining the list of models to run. The model "Gaussian_pk_Lk_C" 
  is called by default (see mixmodGaussianModel() in Rmixmod 
  package to specify other models).}

\item{regModel}{list of character defining the covariance matrix forms for
  the regression of \eqn{U} variables on the \eqn{R} set of variables  
  to select the best model. The best model is the one with
  the highest criterion value. Possible values: "LI" for spherical form,
  "LB" for diagonal form and  "LC" for general form.
  Possible values: "LI", "LB", "LC", c("LI", "LB"), c("LI", "LC"), c("LB", "LC")
  and c("LI", "LB", "LC"). Default is c("LI", "LB", "LC").}

\item{indepModel}{list of character defining the covariance matrix forms for
  independent variables \eqn{W} to select the best model. The best model is the one with
  the highest criterion value. Possible values: "LI" for spherical form and "LB" for diagonal form.
  Possible values: "LI", "LB", c("LI", "LB"). Default is c("LI", LB").}

}

\value{ for each criterion BIC or ICL
\item{nbCluster}{the estimated number of clusters}
\item{proba }{matrix containing the conditional probabilities of belonging to each cluster for all observations}
\item{partition}{vector of length \emph{n} containing the cluster assignments of the \emph{n} observations according to the Maximum-a-Posteriori principle}
\item{model }{The selected covariance model} %% the selected gaussian mixture form
\item{S }{The set of clustering relevant variables}
\item{R }{The subset of regressors}
\item{U }{The set of redundant variables}
\item{W }{The set of independent variables}
\item{criterionValue}{The criterion value for the selected model}
\item{regModel }{The  selected covariance form for the regression}
\item{indepModel}{The selected covariance form for the independent variables}
}


\author{
Mohammed Sedki <\url{mohammed.sedki@u-psud.fr}>
}
\references{
}

\keyword{model-based clustering, discriminant analysis, variable selection, lasso ranking and graphical lasso}
\seealso{
à venir le papier
}
\examples{
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
 
}
