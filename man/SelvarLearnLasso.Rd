\name{SelvarLearnLasso}
\alias{SelvarLearnLasso}
\title{
Regularization for variable selection in discriminant analysis 
}
\description{
This function implements the variable selection in discriminant 
analysis using a lasso ranking on the variables as described in Celeux et al (2014).
The variable ranking step uses the penalized EM algorithm of Zhou et al (2009) (supervised case).
}
\usage{
SelvarLearnLasso(data,
                 lambda, 
                 rho, 
                 hybrid.size,  
                 models, 
                 regModel, 
                 indepModel,
                 knownlabels)
}

\arguments{

  \item{data}{
  matrix  containing quantitative data. 
  Rows correspond to observations and
  columns correspond to variables.
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

%\item{criterion}{character defining the criterion
%  to select the best model. The selected model is the one with
%  the lowest criterion value. Possible values: "BIC".}

\item{models}{a  Rmixmod [\code{\linkS4class{Model}}] object
  defining the list of models to run. For quantitative
  data, the model "Gaussian_pk_Lk_C" is called (see
  mixmodGaussianModel() in Rmixmod package to specify other models).}

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

\item{knownlabels}{
  an integer vector or a factor of size
  number of observations. Each cell corresponds to a
  cluster affectation. So the maximum value is the number
  of clusters.
}
}

\value{ 
\item{nbCluster}{number of clusters}
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
  Zhou, H., Pan, W., and Shen, X., 2009. "Penalized model-based 
  clustering with unconstrained covariance matrices". 
  Electronic Journal of Statistics, vol. 3, pp.1473–1496.
  
  Maugis, C., Celeux, G., and Martin-Magniette, M. L., 2009. 
  "Variable selection in model-based clustering: 
  A general variable role modeling". Computational 
  Statistics and Data Analysis, vol. 53/11, pp. 3872–3882.

   Celeux, G., Martin-Magniette, M. L., Maugis, C., and Raftery, A. E., 2014. 
  "Comparing Model Selection and Regularization Approaches to Variable
   Selection in Model-Based Clustering". Journal de la Société Française
   de Statistique, vol. 155/2, pp. 57–71.

}

\keyword{discriminant analysis, variable selection, lasso ranking and graphical lasso}
\seealso{
\link{SelvarClustLasso}
\link{SortvarLearn}
\link{SortvarClust}
\link{scenarioCor}
}
\examples{

## Simulated data  example as shown in Celeux et al. (2014) (correlated scenario 2) 
## n = 2000 observations, p = 14

data(scenarioCor)

lambda <- seq(0.1,  100, length = 50)
rho <- seq(1, 2, length=2)
hybrid.size <- 3
models <- mixmodGaussianModel(family = "spherical", equal.proportions = TRUE)
regModel <- c("LI","LB","LC")
indepModel <- c("LI","LB")

##supervised clustering (discriminant analysis)
data.cor <- scenarioCor[,1:14]
labels.cor <-scenarioCor[,15]
simulate.learn <- SelvarLearnLasso(data.cor, lambda, rho, hybrid.size, models, regModel, indepModel, labels.cor)



}
