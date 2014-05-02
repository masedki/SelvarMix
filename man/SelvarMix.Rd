\name{SelvarMix}
\alias{SelvarMix}
%\alias{SelvarMixWrapper}
\title{
Regularization for variable selection in model-based clustering
and discriminant analysis
}
\description{
These functions implement the variables selection in model-based clustering or
discriminant analysis using a lasso ranking on the variables as described in Celeux et al (2014).
The variables ranking step uses the penalized EM algorithm of Zhou et al (2009).
}
\usage{
SelvarMix(data, 
          nbCluster, 
          lambda, 
          rho, 
          pack.size = 3,  
          criterion = "BIC", 
          models, 
          modelReg = c("LI", "LB", "LC"), 
          modelIndep = c("LI", "LB"), 
          supervised = FALSE, 
          knownlabels = NULL)
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
\item{pack.size}{
optionnal parameter make less strength  the hybrid forward and backward 
algorithms to select \eqn{S} and \eqn{W} sets.
}

\item{criterion}{list of character defining the criterion
  to select the best model. The best model is the one with
  the lowest criterion value. Possible values: "BIC",
  "ICL", c("BIC", "ICL"). Default is "BIC".}

\item{models}{a  Rmixmod [\code{\linkS4class{Model}}] object
  defining the list of models to run. For quantitative
  data, the model "Gaussian_pk_Lk_C" is called (see
  mixmodGaussianModel() in Rmixmod package to specify other models).}

\item{modelReg}{list of character defining the covariance matrix forms for
  the regression of \eqn{U} variables on the \eqn{R} set of variables  
  to select the best model. The best model is the one with
  the highest criterion value. Possible values: "LI" for spherical form,
  "LB" for diagonal form and  "LC" for general form.
  Possible values: "LI", "LB", "LC", c("LI", "LB"), c("LI", "LC"), c("LB", "LC")
  and c("LI", "LB", "LC"). Default is c("LI", "LB", "LC").}

\item{modelIndep}{list of character defining the covariance matrix forms for
  independent variables \eqn{W} to select the best model. The best model is the one with
  the highest criterion value. Possible values: "LI" for spherical form and "LB" for diagonal form.
  Possible values: "LI", "LB", c("LI", "LB"). Default is c("LI", LB").}


\item{supervised}{
TRUE for discriminant analysis (FALSE by default).
}

\item{knownlabels}{
  an integer vector or a factor of size
  number of observations. Each cell corresponds to a
  cluster affectation. So the maximum value is the number
  of clusters. (NULL by default).
  
}
}

\value{ for each criterion BIC or ICL
\item{nbCluster}{the estimated number of clusters}
\item{proba }{matrix containing the conditional probabilities of belonging to each cluster for all observations}
\item{partition}{vector of length \emph{n} containing the cluster assignments of the \emph{n} observations according to the MAP}
\item{m }{The best covariance model} %% the selected gaussian mixture form
\item{S }{The set of clustering relevant variables}
\item{R }{The subset of regressors}
\item{U }{The set of redundant variables}
\item{W }{The set of independent variables}
\item{crit}{The criterion value for the best model}
\item{r }{The best covariance form for the regression}
\item{l}{The best covariance form for the independent variables}
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
simulate.2 <- SelvarMix(data, nbCluster, lambda, rho, pack.size, criterion, models, modelReg, modelIndep, supervised = TRUE, knownlabels = label)
}
