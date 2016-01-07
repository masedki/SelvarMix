clustemglasso <- function(reference)
{
   
  
  if((length(reference@strategy@lambda)*length(reference@strategy@rho)) < reference@strategy@nbcores)
    reference@strategy@nbcores <- length(reference@strategy@lambda)*length(reference@strategy@rho)
  
  if(Sys.info()["sysname"] == "Windows")
    cl <- makeCluster(reference@strategy@nbcores)
  
  if(length(reference@model@g) == 1)
    junk <- initparameter(scale(reference@data@x, TRUE, TRUE), reference@model@g, n.start = 250, small.pen = 0.5) 
  else{  
    
    wrapperinit <- function(k)
    {
     return(initparameter(scale(reference@data@x, TRUE, TRUE), k, n.start = 250, small.pen = 0.5))
    }
    
    
    if(Sys.info()["sysname"] == "Windows")
    {
      common.objects <- c("initparameter")
      clusterEvalQ(cl, require(glasso))
      clusterExport(cl=cl, varlist = common.objects, envir = environment())
      junk <- clusterApply(cl, x = as.integer(reference@mode@g), fun = wrapperinit)
    }
    else
      junk <- mclapply(X = as.integer(reference@model@g), 
                       FUN = wrapperinit, 
                       mc.cores = reference@strategy@nbcores,
                       mc.preschedule = TRUE,
                       mc.cleanup = TRUE)
    
    
  }
  
  
  wrapperclustemglasso <- function(prm)
  {
    result <- rcppclustemglasso(P, prm[1], prm[2])
    return(result)
  }
  
  #pen.grid <- matrix(0, (length(reference@strategy@lambda)*length(reference@strategy@rho)), 2)  
  pengrid <- matrix(0,0,0)
  pengrid <- as.matrix(expand.grid(reference@strategy@lambda, reference@strategy@rho))
   
  varrole <- array(0,dim=c((length(reference@strategy@lambda)*length(reference@strategy@rho)), reference@data@d, length(reference@model@g)))
  parallelvarrole <- list()
  if(length(reference@model@g)==1)
  {
    P <- junk
    ## si c'est sous windows
    if(Sys.info()["sysname"] == "Windows")
    {
      common.objects <- c("P") 
      clusterEvalQ(cl, require(glasso))
      clusterExport(cl=cl, varlist = common.objects, envir = environment())
      parallelvarrole[[1]] <-  parApply(cl, 
                                        X = pengrid,
                                        MARGIN =  1,
                                        FUN = wrapperclustemglasso)  
      
    }
    else
      parallelvarrole[[1]] <-  mclapply(X = as.list(data.frame(t(pengrid))), 
                                         FUN = wrapperclustemglasso,
                                         mc.cores = reference@strategy@nbcores,
                                         mc.preschedule = TRUE,
                                         mc.cleanup = TRUE)
  }
  else
    for(k in 1:length(reference@model@g))
    {
      P <- junk[[k]]
      if(Sys.info()["sysname"] == "Windows")
      {
        common.objects <- c("P")
        clusterEvalQ(cl, require(glasso))
        clusterExport(cl=cl, varlist = common.objects, envir = environment())
        parallelvarrole[[k]] <- parApply(cl, 
                                         X = pengrid,
                                         MARGIN =  1,
                                         FUN = wrapperclustemglasso)
      }
      else
        parallelvarrole[[k]] <- mclapply(X = as.list(data.frame(t(pengrid))),
                                         FUN = wrapperclustemglasso,
                                         mc.cores = reference@strategy@nbcores,
                                         mc.preschedule = TRUE,
                                         mc.cleanup = TRUE)
    } 
  ## si je suis sous windows
  if(Sys.info()["sysname"] == "Windows")
    stopCluster(cl)
  for(k in 1:length(reference@model@g))
  {
    varrole <- matrix(NA,(length(reference@strategy@lambda)*length(reference@strategy@rho)), reference@data@d)
    for(j in 1:nrow(varrole))
      if(class(parallelvarrole[[k]][[j]])!="try-error")
        varrole[j,] <- parallelvarrole[[k]][[j]]   
      
      varrole[,,k] <- varrole
  }
  matrix0 <- matrix(0, nrow=length(reference@model@g), ncol=reference@data@d)
  for(k in 1:length(reference@model@g))
    matrix0[k,]<- colSums(varrole[,,k])    
  
  ordervar <- matrix(NA, nrow=length(reference@model@g),ncol=reference@data@d)
  for(k in 1:length(reference@model@g))
    ordervar[k,] <- sort.int(matrix0[k,],decreasing=TRUE,index.return=TRUE)$ix
  reference@model@rank <- ordervar
  return(reference)
}







