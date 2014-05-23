\name{SortvarClust}
\alias{SortvarClust}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Variable ranking with LASSO in model-based clustering  
}
\description{
This function implements variable ranking procedure in model-based clustering  
using the penalized EM algorithm of Zhou et al (2009).
}
\usage{
SortvarClust(data, nbCluster, lambda, rho)
}
\arguments{
  \item{data}{
  matrix  containing quantitative data. 
  Rows correspond to observations and
  columns correspond to variables
  }
 
 \item{nbCluster}{
  numeric listing the number of clusters (must be integers)
  }
  
  \item{lambda}{
   numeric listing of tuning parameter for \eqn{\ell_1} mean penalty
  }
  
  
  \item{rho}{
  numeric listing of tuning parameter for \eqn{\ell_1} precision matrix penalty
  }

}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  matrix with rows corresponding to variable raking. Each 
  row corresponds to a value in nbCluster.
}
\references{
  Zhou, H., Pan, W., and Shen, X., 2009. "Penalized model-based 
  clustering with unconstrained covariance matrices". 
  Electronic Journal of Statistics, vol. 3, pp.1473-1496.
  
  Maugis, C., Celeux, G., and Martin-Magniette, M. L., 2009. 
  "Variable selection in model-based clustering: 
  A general variable role modeling". Computational 
  Statistics and Data Analysis, vol. 53/11, pp. 3872-3882.
}
\author{
Mohammed Sedki <mohammed.sedki@u-psud.fr>
}
%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{SortvarLearn}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Simulated data  example as shown in Maugis et al. (2014) 
## n = 2000 observations, p = 14 variables 

data(scenarioCor)
data.cor <- scenarioCor[,1:14]


lambda <- seq(0.1,  100, length = 50)
rho <- seq(1, 2, length=2)
nbCluster <-  4

## variable ranking in model-based clustering  
var.ranking.clust <- SortvarClust(data.cor, nbCluster, lambda, rho)
}
\keyword{Variable ranking}
\keyword{Penalized model-based clustering}

