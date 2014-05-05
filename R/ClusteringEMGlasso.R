ClusteringEMGlasso <- function(data, 
                               nbCluster, 
                               lambda, 
                               rho,
                               parallel = TRUE){
  data <- as.matrix(data)
  n <- as.integer(dim(data)[1])
  p <- as.integer(dim(data)[2])
  nbCluster <- as.integer(nbCluster)
  nb.cpus <- detectCores(all.tests = FALSE, logical = FALSE)
  
  if(nb.cpus == 1 || nb.cpus == 2)
    cl <- makeCluster(nb.cpus)
  else
  {
    if((length(lambda)*length(rho)) < nb.cpus)
      cl <- makeCluster((length(lambda)*length(rho)))
    else
      cl <- makeCluster(nb.cpus-1)
  }
  
  if(length(nbCluster) == 1)
  {
    junk <- InitParameter(data, nbCluster, n.start = 250, small.pen = 0.5)
    ##print("ClusteringEMGlasso ....... Initialization ....... Done ")
    
  }
    else{  
    wrapper.init.parameter <- function(k){return(InitParameter(data, k, n.start = 250, small.pen = 0.5))}
    clusterEvalQ(cl, require(glasso))
    common.objects <- c("InitParameter")
    clusterExport(cl=cl, varlist = common.objects, envir = environment())
    
    junk <- clusterApply(cl, x = as.integer(nbCluster), fun = wrapper.init.parameter)
    ##print("ClusteringEMGlasso ....... Initialization ....... Done ")
    
  }
  

  wrapper.clusteringEMGlasso <- function(prm)
  {
    result <- try(rcppClusteringEMGlasso(P, prm[1], prm[2]), silent = TRUE)              
    return(result)
  }
  ##...................................................................##
  ##....ici junk[[k]] contient le paramètre initial pour nbClust = k....##
  ##...................................................................##
  pen.grid <- matrix(0, (length(lambda)*length(rho)), 2)  
  pen.grid <- as.matrix(expand.grid(lambda, rho))
  pen.grid.list <- list(); colnames(pen.grid) <- NULL
  pen.grid.list <- as.list(data.frame(t(pen.grid)))
  
 VarRole <- array(0,dim=c((length(lambda)*length(rho)), p, length(nbCluster)))
  parallel.varrole <- list()
  if(length(nbCluster)==1)
  {
    P <- junk
    clusterEvalQ(cl, require(glasso))
    common.objects <- c("P") 
    clusterExport(cl=cl, varlist = common.objects, envir = environment())
    parallel.varrole[[1]] <-  parLapply(cl, pen.grid.list, wrapper.clusteringEMGlasso)
  
  }
  else
  for(k in 1:length(nbCluster))
  {
    P <- junk[[k]]
    clusterEvalQ(cl, require(glasso))
    common.objects <- c("P")  
    clusterExport(cl=cl, varlist = common.objects, envir = environment())
    parallel.varrole[[k]] <- parLapply(cl, pen.grid.list, wrapper.clusteringEMGlasso)
   } 
  stopCluster(cl)
  
  for(k in 1:length(nbCluster))
  {
    var.role <- matrix(0,(length(lambda)*length(rho)), p)
    for(j in 1:nrow(var.role))
      if(class(parallel.varrole[[k]][[j]])!="try-error")
        var.role[j,] <- parallel.varrole[[k]][[j]]   
    
    VarRole[,,k] <- var.role
  }
  
 return(VarRole)
}







