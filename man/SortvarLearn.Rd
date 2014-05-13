\name{SortvarLearn}
\alias{SortvarLearn}
\title{
Variable ranking with LASSO in discriminant analysis 
}
\description{
This function implements variable ranking procedure in discriminant analysis 
using the penalized EM algorithm of Zhou et al (2009).
}
\usage{
SortvarLearn(data, lambda, rho, knownlabels)
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
  \item{knownlabels}{
  an integer vector or a factor of size
  number of observations. Each cell corresponds to a
  cluster affectation. So the maximum value is the number
  of clusters.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
 vector of integers corresponding to variable raking.
}
\references{
  Zhou, H., Pan, W., and Shen, X., 2009. "Penalized model-based 
  clustering with unconstrained covariance matrices". 
  Electronic Journal of Statistics, vol. 3, pp.1473–1496.
}
\author{
Mohammed Sedki \email{mohammed.sedki@u-psud.fr}
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{SortvarClust}
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
labels.cor <-scenarioCor[,15]

## variable ranking in discriminant analysis 
labels.cor <-scenarioCor[,15]
var.ranking.learn <- SortvarLearn(data.cor, lambda, rho, labels.cor)
}
\keyword{Variable ranking}
\keyword{Penalized discriminant analysis}
