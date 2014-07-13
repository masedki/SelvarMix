##################################################################
##          VariableSelection.R
##################################################################

##  INPUT
# data (dataframe)    matrix of data
# nbCluster (vector)  values of K
# models    VOIR COMMENT HARMONISER AVEC RMIXMOD
# modelReg  (vector)  LI, LB and/or LC   (par d?faut les trois si l'utilisateur ne donne rien)
# criterion (vector)  BIC and/or ICL     (par d?faut BIC)
# OrderVariable   (matrix) matrice contenant l'ordre des variables pour chaque valeur de K, r?sultat du programme SortVariableLasso.R
#  pas ...

## OUTPUT
#  reflechir au stockage de (S,R,U,W) pour chaque triplet (K,m,r)
VariableSelection<-
  function(data,
           nbCluster,
           models,
           criterion,
           OrderVariable,
           hybrid.size,
           supervised,
           knownlabels)
  {
    data <- as.matrix(data)
    nbCluster <- as.integer(nbCluster)
    criterion <- as.character(criterion)
    
    
    listModels.size <- length(models["listModels"])
    nbCluster.size <- length(nbCluster)
    OutputVector.size <- listModels.size*nbCluster.size
    
    wrapper.selectVar <- function(arg)
    {
      myModel <-  mixmodGaussianModel(listModels=models["listModels"][arg[2]])
      orderVar <- rep(NA, ncol(data))
      
      if(length(nbCluster)==1)
        orderVar <- OrderVariable 
      else
        orderVar <- OrderVariable[arg[1],]
      
      my.list <- try(rcppSelectS(data, 
                                 orderVar, 
                                 nbCluster[arg[1]], 
                                 myModel, 
                                 hybrid.size, 
                                 criterion,
                                 knownlabels,
                                 supervised), silent = TRUE)
      if(class(my.list)!="try-error")
      {
        ResSelectVar <- list()
        ResSelectVar$S <- my.list$S
        ResSelectVar$nbCluster <- my.list$nbCluster
        ResSelectVar$criterion <- my.list$criterion
        ResSelectVar$model <- my.list$model 
        ResSelectVar$criterionValue  <- my.list$criterionValue
        ResSelectVar$partition <- my.list$partition
        ResSelectVar$proba <- my.list$proba
        OrderAux <- setdiff(OrderVariable, ResSelectVar$S)
        ResSelectVar$W <- rcppSelectW(data, OrderAux, ResSelectVar$S, hybrid.size)
        return(ResSelectVar)
      }
      else
        return(my.list)
    }
    
    nb.cpus <- detectCores(all.tests = FALSE, logical = FALSE)
    if(nb.cpus == 1 || nb.cpus == 2)
      cl <- makeCluster(nb.cpus)
    else 
    {
      if(OutputVector.size < nb.cpus)
        cl <- makeCluster(OutputVector.size) 
      else
        cl <- makeCluster(nb.cpus-1)
      
    }
    
    arg.grid <- matrix(0, OutputVector.size, 2)  
    arg.grid <- as.matrix(expand.grid(1:nbCluster.size, 1:listModels.size))
    colnames(arg.grid) <- NULL
    
    clusterEvalQ(cl, require(Rmixmod))
    common.objects <- c("data", 
                        "OrderVariable", 
                        "nbCluster",
                        "models",
                        "hybrid.size", 
                        "criterion",
                        "supervised",
                        "knownlabels")
    clusterExport(cl=cl, varlist = common.objects, envir = environment())
    junk <- parRapply(cl, x=arg.grid, FUN = wrapper.selectVar)
    stopCluster(cl)
    
    
    
    ## je vais compter le nombre d'échecs 
    nb.fails <- 0 
    for(idx in 1:OutputVector.size)
      if(class(junk[[idx]]) == "try-error")
        nb.fails <- nb.fails + 1
        
    ##Préparer le stockage
    VariableSelectRes <-  vector(length = (OutputVector.size - nb.fails), mode ="list")
    
    idx <- 1
    for(ll in 1:OutputVector.size)
      if(class(junk[[ll]])!="try-error")
      {
        VariableSelectRes[[idx]]$S <- sort(junk[[ll]][["S"]])
        VariableSelectRes[[idx]]$W <- sort(junk[[ll]][["W"]])
        VariableSelectRes[[idx]]$U <- setdiff(1:dim(data)[2], union(junk[[ll]][["S"]], junk[[ll]][["W"]]))
        VariableSelectRes[[idx]]$criterionValue <- junk[[ll]][["criterionValue"]]
        VariableSelectRes[[idx]]$criterion <- junk[[ll]][["criterion"]] 
        VariableSelectRes[[idx]]$model <- junk[[ll]][["model"]]
        VariableSelectRes[[idx]]$nbCluster <- junk[[ll]][["nbCluster"]]
        VariableSelectRes[[idx]]$partition <- junk[[ll]][["partition"]]
        VariableSelectRes[[idx]]$proba <- junk[[ll]][["proba"]]
        idx <- idx + 1
      }
    
    return(VariableSelectRes)  
  }


