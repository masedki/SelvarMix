######################################################
##        ModelSelectionClust.R
######################################################
ModelSelectionClust <- function(VariableSelectRes,
                                data,
                                regModel,
                                indepModel)
{
  
  
  
  ## je récupère le nombre d'éléments de la liste
  mylist.size <- length(VariableSelectRes)
  if(mylist.size==1)
    junk <- try(rcppCrit(data, 
                         VariableSelectRes, 
                         regModel, 
                         indepModel), silent = TRUE)
  else
  {
    wrapper.rcppCrit <- function(idx)
    {
      mylist <- VariableSelectRes[[idx]]
      res <- rcppCrit(data, 
                      mylist,
                      regModel, 
                      indepModel)
      
      return(res)
    }
    
    nb.cpus <- detectCores(all.tests = FALSE, logical = FALSE)
    nb.cores <- NA
    if(nb.cpus == 1 || nb.cpus == 2)
      nb.cores <- nb.cpus
    else 
    {
      if(mylist.size < nb.cpus) 
        nb.cores <- mylist.size
      else
        nb.cores <- nb.cpus - 1
      
    }
    
    ## si on est sous windows 
    if(Sys.info()["sysname"] == "Windows")
    {
      cl <- makeCluster(nb.cores)
      common.objects <- c("data", "VariableSelectRes", "regModel", "indepModel")
      clusterExport(cl=cl, varlist = common.objects, envir = environment())
      junk <- clusterApply(cl, 
                           x = as.integer(1:mylist.size), 
                           fun = wrapper.rcppCrit)
      stopCluster(cl)
      
    }
    else
      junk <- mclapply(X = as.integer(1:mylist.size), 
                       FUN = wrapper.rcppCrit,
                       mc.cores = nb.cores,
                       mc.preschedule = TRUE,
                       mc.cleanup = TRUE)
  } 
  
  
  if((mylist.size==1) && (class(junk) != "try-error"))
    bestModel <- junk
  else
  { 
    lmax <- -Inf
    for(idx in 1:mylist.size)
    {
      if((class(junk[[idx]]) != "try-error")  && (junk[[idx]]$criterionValue > lmax))
      {
        bestModel <- junk[[idx]]
        lmax <- bestModel$criterionValue 
      }
    }
  }
  
  
  if(length(bestModel$R) == 0)
  {
    bestModel$R <- NULL
    bestModel$W <- c(bestModel$U, bestModel$W)
    bestModel$U <- NULL
  }
  
  if(length(bestModel$W)==0)
    bestModel$W <- NULL
  
  
  
  return(bestModel)
}