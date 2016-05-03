modelsel <- function(varselres, criterion,reference)
{
  outputsize <- length(varselres)
  if(outputsize==1)
    junk <- try(rcppCrit(reference@data@x, varselres, reference@strategy@rmodel, reference@strategy@imodel), silent = TRUE)
  else
  {
    wrapperrcppcrit <- function(idx)
    {
      return(rcppCrit(reference@data@x, varselres[[idx]], reference@strategy@rmodel, reference@strategy@imodel))
    }
    if(outputsize < reference@strategy@nbcores) 
       reference@strategy@nbcores <- outputsize
    
    if(Sys.info()["sysname"] == "Windows")
    {
      cl <- makeCluster(reference@strategy@nbcores)
      commonobjects <- c("reference", "varselres")
      clusterExport(cl=cl, varlist = commonobjects, envir = environment())
      junk <- clusterApply(cl, x = 1:outputsize, fun = wrapperrcppcrit)
      stopCluster(cl)
      
    }
    else
      junk <- mclapply(X = 1:outputsize, FUN = wrapperrcppcrit, mc.cores = reference@strategy@nbcores, mc.preschedule = TRUE, mc.cleanup = TRUE)
  } 
  bestModel <- list()
  if((outputsize==1) && (class(junk) != "try-error"))
    bestModel <- junk
  else
  { 
    lmax <- -Inf
    for(idx in 1:outputsize)
    {
      #print(junk[[idx]])
      if((class(junk[[idx]]) != "try-error")  && (junk[[idx]]$criterionValue > lmax))
      {
        bestModel <- junk[[idx]]
        lmax <- bestModel$criterionValue
        print(c("bestModel$criterionValue = ...", bestModel$criterionValue))
      }
    }
  }
  
  
  if(criterion == "BIC")
    reference@criteria@BIC <- bestModel$criterionValue
  else
    reference@criteria@ICL <- bestModel$criterionValue
  
  reference@model@g <-  bestModel$nbCluster
  reference@model@S <-  bestModel$S
  reference@partition@zMAP <- bestModel$partition
  reference@tik <- bestModel$proba
  
  if(length(bestModel$R) == 0)
  {
    reference@output@model@R <- NULL
    reference@output@model@W <- c(bestModel$U, bestModel$W)
    reference@output@model@U <- NULL
  }
  
  if(length(bestModel$W)==0)
    reference@output@model@W <- NULL
  
  
  
  return(reference)
}