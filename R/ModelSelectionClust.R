######################################################
##        ModelSelectionClust.R
######################################################

##  INPUT
# VariableSelectRes   resultats de la selection de variables obtenus avec VariableSelection.R
# data (dataframe)    matrix of data
# nbCluster (vector)  values of K
# models    VOIR COMMENT HARMONISER AVEC RMIXMOD
# modelReg  (vector)  LI, LB and/or LC   (par d?faut les trois si l'utilisateur ne donne rien)
# modelIndep (vector) LI and/or LB       (par d?faut les deux si l'utilisateur ne donne rien)
# criterion (vector)  BIC and/or ICL     (par d?faut BIC)
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
      res <- try(rcppCrit(data, 
                          mylist,
                          regModel, 
                          indepModel), silent = TRUE)
      return(res)
    }
    
    nb.cpus <- detectCores(all.tests = FALSE, logical = FALSE)
    if(nb.cpus == 1 || nb.cpus == 2)
      cl <- makeCluster(nb.cpus)
    else 
    {
      if(mylist.size < nb.cpus)
        cl <- makeCluster(mylist.size) 
      else
        cl <- makeCluster(nb.cpus-1)
      
    }
    
    common.objects <- c("data", "VariableSelectRes", "regModel", "indepModel")
    clusterExport(cl=cl, varlist = common.objects, envir = environment())
    junk <- clusterApply(cl, x = as.integer(1:mylist.size), fun = wrapper.rcppCrit)
    stopCluster(cl)
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