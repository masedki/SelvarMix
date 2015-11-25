setClass(
  Class = "SelvarMixpartitions", 
  representation = representation(zMAP="numeric",  tik="matrix"), 
  prototype = prototype(zMAP=numeric(), tik=matrix(0,0,0))
)

setClass(
  Class = "SelvarMixstrategy", 
  representation = representation(lambda="numeric", rho="numeric", hsize="numeric", criterion="character", models="Strategy", regModel="character", indepModel="character", nbCores ="numeric"), 
  prototype = prototype(lambda=numeric(), rho=numeric(), hsize=numeric(), criterion=character(), models=new("Strategy"), regModel=character(), indepModel=character(), nbCores = numeric())
) 

## Constructeur de la classe S4 SelvarMixstrategy
SelvarMixstrategy <- function(lambda, rho, hsize, criterion, models, rmodel, imodel, nbcores){
  #if( nbKeep > nbSmall)
  #  nbKeep <- nbSmall
  new("SelvarMixstrategy", lambda=lambda, rho=rho, hsize=hsize, criterion=criterion, models=models, rmodel=regModel, imodel=imodel, nbcores=nbcores)
}


setClass(
  Class = "SelvarMixmodel", 
  representation = representation(g="integer", rank= "matrix",S="numeric", R="numeric", U="numeric", W="numeric", m="character", l="character", r="character"), 
  prototype = prototype(g=integer(), rank = matrix(),S=numeric(), R=numeric(), U=numeric(), W=numeric(), m=character(), l=character(), r=character())
)

# setClass(
#   Class = "ClustOrddetailsMH", 
#   representation = representation(Bestmodel="matrix", Currentmodel="matrix", Candidatemodel="matrix", allbic="matrix"), 
#   prototype = prototype(Bestmodel=matrix(), Bestmodel=matrix(), Bestmodel=matrix())
# )


## voir comment recuperer les bonnes valeurs
setClass(Class = "SelvarMixcriteria", 
         representation = representation(loglikelihood="numeric", BIC="numeric", ICL="numeric", nbparam="numeric"), 
         prototype = prototype(loglikelihood=numeric(), BIC=numeric(), ICL=numeric(), nbparam=numeric())
)
## voir comment recuperer tout les parametres

setClass(
  Class = "SelvarMixparam", 
  representation = representation(pi="numeric", alpha="list", beta="list", epsilon="matrix"), 
  prototype = prototype(pi=numeric(), alpha=list(), beta=list(), epsilon=matrix())
)


## attention entre Clust et Learn
setClass(
  Class = "SelvarMixdata", 
  representation = representation(n="numeric", d="numeric", x="matrix", z ="numeric", xt="matrix", zt="numeric"), 
  prototype = prototype(n=numeric(), d=numeric(), x=matrix(), z=numeric(), xt=matrix(), zt=numeric())
)

setClass(
  Class = "SelvarMixresults", 
  representation = representation(data="SelvarMixdata", criteria="SelvarMixcriteria", partitions="SelvarMixpartitions",
                                  model="SelvarMixmodel", strategy="SelvarMixstrategy", param="SelvarMixparam"), 
  prototype = prototype(data=new("SelvarMixdata"), criteria=new("SelvarMixcriteria"), partitions=new("SelvarMixpartitions"),
                        model=new("SelvarMixmodel"), strategy=new("SelvarMixstrategy"), param=new("SelvarMixparam"))
)

BuildS4object1 <- function(x, 
                          nbcluster, 
                          lambda, 
                          rho, 
                          hsize, 
                          criterion, 
                          models, 
                          rmodel, 
                          imodel, 
                          nbcores, 
                          learn=FALSE,
                          z=NULL,
                          xt=NULL,
                          zt=NULL
                          )
{
  
  if(!learn)
  data <- new("SelvarMixdata", n=nrow(x), d=ncol(x), x=x, z = 1:nrow(x), xt=NULL, zt=NULL)
  else
  data <- new("SelvarMixdata", n=nrow(x), d=ncol(x), x=x, z = z, xt=xt, zt=zt)
  
  strategy <- SelvarMixstrategy(lambda, rho, hsize, criterion, models, rmodel, imodel, nbcores)
  output <- new("SelvarMixresults", 
                data=data, 
                criteria=new("SelvarMixcriteria", loglikelihood=-Inf, BIC=-Inf, ICL=-Inf, nbparam=0),
                strategy=strategy, 
                model=new("SelvarMixmodel", 
                          g=nbcluster, 
                          rank = matrix(0,0,0),
                          S=sample(1:(ncol(x)), ncol(x), rep=T), 
                          R=sample(1:(ncol(x)), ncol(x), rep=T), 
                          U=sample(1:(ncol(x)), ncol(x), rep=T), 
                          W=sample(1:(ncol(x)), ncol(x), rep=T),
                          m="NA", 
                          l="NA", 
                          r="NA"))
  return(output)
}
BuildS4object2 <- function(x,
                           nbcluster, 
                           lambda, 
                           rho, 
                           nbcores, 
                           learn,
                           z
)
{
  
  if(!learn)
    data <- new("SelvarMixdata", n=nrow(x), d=ncol(x), x=x)
  else
    data <- new("SelvarMixdata", n=nrow(x), d=ncol(x), x=x, z = z)
  
  strategy <- new("SelvarMixstrategy", lambda=lambda, rho=rho, nbcores=nbcores)
  output <- new("SelvarMixresults", 
                data=data, 
                criteria=new("SelvarMixcriteria", loglikelihood=-Inf, BIC=-Inf, ICL=-Inf, nbparam=0),
                strategy=strategy, 
                model=new("SelvarMixmodel", g=nbcluster))
  return(output)
}
