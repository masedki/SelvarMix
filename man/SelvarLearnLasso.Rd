\name{SelvarLearnLasso}
\alias{SelvarLearnLasso}
\title{
Regularization for variable selection in discriminant analysis 
}
\description{
This function implements the variable selection in discriminant 
analysis using a lasso ranking on the variables as described in !!!! OUR PAPER!!!.
The variable ranking step uses the penalized EM algorithm of Zhou et al (2009) (supervised case).
A testing data-labels sample can be used to compute the averaged classification error rates.
}
\usage{
SelvarLearnLasso(data, knownlabels, lambda, rho, hybrid.size,  models, regModel, indepModel,
                 dataTest, labelsTest)
}

\arguments{

  \item{data}{
  matrix  containing quantitative data. 
  Rows correspond to observations and
  columns correspond to variables
}

\item{knownlabels}{
  an integer vector or a factor of size
  number of observations. Each cell corresponds to a
  cluster affectation. So the maximum value is the number
  of clusters
}

 \item{lambda}{
  numeric listing of tuning parameter for \eqn{\ell_1} mean penalty
}
  \item{rho}{
  numeric listing of tuning parameter for \eqn{\ell_1} precision matrix penalty
}
\item{hybrid.size}{
optionnal parameter make less strength  the hybrid forward and backward 
algorithms to select \eqn{S} and \eqn{W} sets
}

%\item{criterion}{character defining the criterion
%  to select the best model. The selected model is the one with
%  the lowest criterion value. Possible values: "BIC".}

\item{models}{a  Rmixmod [\code{\linkS4class{Model}}] object
  defining the list of models to run. The models 
  Gaussian_pk_L_C, Gaussian_pk_Lk_C, Gaussian_pk_L_Ck, 
  and Gaussian_pk_Lk_Ck are called by default 
  (see mixmodGaussianModel() in Rmixmod package to
  specify other models)}

\item{regModel}{list of character defining the covariance matrix form for
  the linear regression of \eqn{U} on the \eqn{R} set of variable. 
  Possible values: "LI" for spherical form, "LB" for diagonal form and  
  "LC" for general form. Possible values: "LI", "LB", "LC", c("LI", "LB")
  , c("LI", "LC"), c("LB", "LC") and c("LI", "LB", "LC"). 
  Default is c("LI", "LB", "LC")}

\item{indepModel}{list of character defining the covariance matrix form for
  independent variables \eqn{W}. Possible values: 
  "LI" for spherical form and "LB" for diagonal form.
  Possible values: 
  "LI", "LB", c("LI", "LB"). Default is c("LI", LB")}


\item{dataTest}{matrix  containing quantitative testing data. 
  Rows correspond to observations and
  columns correspond to variables. 
}
\item{labelsTest}{
  an integer vector or a factor of size
  number of testing observations. Each cell corresponds to a
  cluster affectation. 
}
}

\value{ 
%\item{nbCluster}{number of clusters}
\item{S }{The selected set of clustering relevant variables}
\item{R }{The selected subset of regressors}
\item{U }{The selected set of redundant variables}
\item{W }{The selected set of independent variables}
\item{criterionValue}{The criterion value for the selected model}
\item{model }{The selected covariance model} %% the selected gaussian mixture form
\item{regModel }{The  selected covariance form for the regression}
\item{indepModel}{The selected covariance form for the independent variables}

%\item{proba }{matrix containing the conditional probabilities of belonging to each cluster for all observations}
%\item{partition}{vector of length \emph{n} containing the cluster assignments of the \emph{n} observations according to the Maximum-a-Posteriori principle}

\item{error }{error rate done by the Maximum-A-Posteriori principle}
}


\author{
Mohammed Sedki <\url{mohammed.sedki@u-psud.fr}>
}
\references{
  Zhou, H., Pan, W., and Shen, X., 2009. "Penalized model-based 
  clustering with unconstrained covariance matrices". 
  Electronic Journal of Statistics, vol. 3, pp.1473-1496.
  
  Maugis, C., Celeux, G., and Martin-Magniette, M. L., 2009. 
  "Variable selection in model-based clustering: 
  A general variable role modeling". Computational 
  Statistics and Data Analysis, vol. 53/11, pp. 3872-3882.

  % Celeux, G., Martin-Magniette, M. L., Maugis, C., and Raftery, A. E., 2014. 
  %"Comparing Model Selection and Regularization Approaches to Variable
  % Selection in Model-Based Clustering". Journal de la Soci\'et\'e Fran\,caise
  % de Statistique, vol. 155/2, pp. 57-71.

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
## n = 2000 observations, p = 14 variables 

data(scenarioCor)

lambda <- seq(0.1,  100, length = 50)
rho <- seq(1, 2, length=2)
hybrid.size <- 3
models <- mixmodGaussianModel(family = "spherical", equal.proportions = TRUE)
regModel <- c("LI","LB","LC")
indepModel <- c("LI","LB")

## variables selection in discriminant analysis
## training sample
data <- scenarioCor[1:1000,1:14]
labels <-scenarioCor[1:1000,15]

## testing sample
dataTest <- scenarioCor[1001:2000,1:14]
labelsTest <-scenarioCor[1001:2000,15]

simulate.learn <- SelvarLearnLasso(data, labels, lambda, rho, hybrid.size, models, regModel, indepModel, 
                                   dataTest, labelsTest)



}
