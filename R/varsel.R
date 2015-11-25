varsel<-function(reference, criterion="BIC",learn=FALSE)
  {
    outputsize <- length(reference@strategy@models["listModels"])*length(reference@model@g)
    wrappervarsel <- function(arg)
    {
      mymodel <-  mixmodGaussianModel(listModels=reference@strategy@models["listModels"][arg[2]])
      rank <- rep(NA, reference@data@p)
      
      if(length(reference@model@g)==1)
        rank <- reference@model@rank 
      else
        rank <- reference@model@rank[arg[1],]
      
      mylist <- rcppSelectS(reference@data@x, 
                             rank, 
                             reference@model@g[arg[1]], 
                             mymodel, 
                             reference@strategy@hsize, 
                             criterion,
                             reference@data@z,
                             learn)
      
      ResSelectVar <- list()
      ResSelectVar$S <- mylist$S
      ResSelectVar$g <- mylist$nbCluster
      ResSelectVar$criterion <- mylist$criterion
      ResSelectVar$model <- mylist$model 
      ResSelectVar$criterionValue  <- mylist$criterionValue
      ResSelectVar$partition <- mylist$partition
      ResSelectVar$proba <- mylist$proba
      rankaux <- setdiff(rank, ResSelectVar$S)
      ResSelectVar$W <- rcppSelectW(reference@data@x, rankaux, ResSelectVar$S, reference@strategy@hsize)
      return(ResSelectVar)
    }
    
    if(outputsize < reference@strategy@nbcores)
      reference@strategy@nbcores <- outputsize
    
    arggrid <- matrix(0, outputsize, 2)  
    arggrid <- as.matrix(expand.grid(1:length(reference@model@g), 1:length(reference@strategy@models["listModels"])))
    ## si on est sous windows
    if(Sys.info()["sysname"] == "Windows")
    {
      cl <- makeCluster(reference@strategy@nbcores)
      common.objects <- c("reference", "rank", "learn")
      clusterEvalQ(cl, require(Rmixmod))
      clusterExport(cl=cl, varlist = common.objects, envir = environment())
      junk <- parApply(cl = cl,  
                       X = arggrid,
                       MARGIN = 1,
                       FUN = wrappervarsel)
      stopCluster(cl)
      
    }
    else
      junk <- mclapply(X = as.list(data.frame(t(arggrid))),
                       FUN = wrappervarsel,
                       mc.cores = reference@strategy@nbcores,
                       mc.silent = FALSE,
                       mc.preschedule = TRUE,
                       mc.cleanup = TRUE)
    
    
    ## je vais compter le nombre d'échecs 
    nbfails <- 0 
    for(idx in 1:outputsize)
      if(class(junk[[idx]]) == "try-error")
        nb.fails <- nb.fails + 1
    
    ##Préparer le stockage
    varselres <-  vector(length = (outputsize - nbfails), mode ="list")
    
    idx <- 1
    for(ll in 1:outputsize)
      if(class(junk[[ll]])!="try-error")
      {
        varselres[[idx]]$S <- sort(junk[[ll]][["S"]])
        varselres[[idx]]$W <- sort(junk[[ll]][["W"]])
        varselres[[idx]]$U <- setdiff(1:reference@data@p, union(junk[[ll]][["S"]], junk[[ll]][["W"]]))
        varselres[[idx]]$criterionValue <- junk[[ll]][["criterionValue"]]
        varselres[[idx]]$criterion <- junk[[ll]][["criterion"]] 
        varselres[[idx]]$model <- junk[[ll]][["model"]]
        varselres[[idx]]$nbCluster <- junk[[ll]][["nbCluster"]]
        varselres[[idx]]$partition <- junk[[ll]][["partition"]]
        varselres[[idx]]$proba <- junk[[ll]][["proba"]]
        idx <- idx + 1
      }
    
    return(varselres)  
  }


