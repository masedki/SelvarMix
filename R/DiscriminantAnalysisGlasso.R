##
DiscriminantAnalysisGlasso <- function(data, 
                               nbCluster, 
                               lambda, 
                               rho,
                               parallel = TRUE,
                               knownlabels = NULL){
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
  
  
  wrapper.DiscriminantAnalysisGlasso <- function(prm)
  {
    result <- try(rcppDiscriminantAnalysisGlasso(data, knownlabels, nbCluster, prm[1], prm[2]), silent = TRUE)  
    return(result)
  }
 
  pen.grid <- matrix(0, (length(lambda)*length(rho)), 2)  
  pen.grid <- as.matrix(expand.grid(lambda, rho))
  pen.grid.list <- list(); colnames(pen.grid) <- NULL
  pen.grid.list <- as.list(data.frame(t(pen.grid)))
  
  clusterEvalQ(cl, require(glasso))
  common.objects <- c("data", "nbCluster", "knownlabels") 
  clusterExport(cl=cl, varlist = common.objects, envir = environment())
  parallel.varrole <-  parLapply(cl, pen.grid.list, wrapper.DiscriminantAnalysisGlasso)
  stopCluster(cl)
  
  var.role <- matrix(0,(length(lambda)*length(rho)), p)
  for(j in 1:nrow(var.role))
      if(class(parallel.varrole[[j]])!="try-error")
        var.role[j,] <- parallel.varrole[[j]]   
    
  return(var.role)
}