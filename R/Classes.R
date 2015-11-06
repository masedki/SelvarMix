setClass(
  Class = "SelvarMixpartitions", 
  representation = representation(zMAP="numeric",  tik="matrix"), 
  prototype = prototype(zMAP=numeric(), tik=matrix(0,0,0))
)

setClass(
  Class = "SelvarMixstrategy", 
  representation = representation(lambda="numeric", rho="numeric", hSize="numeric", criterion="character", models="mixmodStrategy", regModel="character", indepModel="character", nbCores ="numeric"), 
  prototype = prototype(lambda=numeric(), rho=numeric(), hSize=numeric(), criterion=character(), models=new("mixmodStrategy"), regModel=character(), indepModel=character(), nbCores = numeric())
) 

## Constructeur de la classe S4 SelvarMixstrategy
SelvarMixstrategy <- function(lambda, rho, hSize, criterion, models, regModel, indepModel, nbCores){
  #if( nbKeep > nbSmall)
  #  nbKeep <- nbSmall
  new("SelvarMixstrategy", lambda=lambda, rho=rho, hSize=hSize, criterion=criterion, models=models, regModel=regModel, indepModel=indepModel, nbCores=nbCores)
}


setClass(
  Class = "SelvarMixmodel", 
  representation = representation(g="numeric", S="numeric", R="numeric", U="numeric", W="numeric"), 
  prototype = prototype(g=numeric(), S=numeric(), R=numeric(), U=numeric(), W=numeric())
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
  Class = "SelvarMix", 
  representation = representation(
    n="numeric",
    d="numeric",
    data="matrix",
    knownlabels ="numeric",
    dataTest="matrix",
    labelsTest="numeric"
    ), 
  prototype = prototype(
    n=numeric(),
    d=numeric(),
    data=matrix(), 
    knownlabels=numeric(),
    dataTest=matrix(),
    labelsTest=numeric()
  )
)

setClass(
  Class = "SelvarMixresults", 
  representation = representation(data="SelvarMixdata", criteria="SelvarMixcriteria", partitions="SelvarMixpartitions",
                                  model="SelvarMixmodel", strategy="SelvarMixstrategy", param="SelvarMixparam"), 
  prototype = prototype(data=new("SelvarMixdata"), criteria=new("SelvarMixcriteria"), partitions=new("SelvarMixpartitions"),
                        model=new("SelvarMixmodel"), strategy=new("SelvarMixstrategy"), param=new("SelvarMixparam"))
)

BuildS4object <- function(x, g, iterMH, nbSmall, iterSmall, nbKeep, iterKeep, tolKeep){
  data <- new("ClustOrddata", n=nrow(x), d=ncol(x), data=x, modalities=max(x)+1)
  strategy <- ClustOrdstrategy(iterMH, nbSmall, iterSmall, nbKeep, iterKeep, tolKeep)
  output <- new("ClustOrdresults", data=data, 
                strategy=strategy, 
                model=new("ClustOrdmodel", g=g, omega=Cleanmodel(sample(1:(ncol(x)), ncol(x), replace = T))[2,]-1),
                criteria=new("ClustOrdcriteria", BIC=-Inf, ICL=-Inf, loglikelihood=-Inf, nbparam=0))
  return(output)
}
