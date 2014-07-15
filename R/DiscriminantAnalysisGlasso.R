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
  nb.cores <- NA
  if(nb.cpus == 1 || nb.cpus == 2)
    nb.cores <- nb.cpus
  else
  {
    if((length(lambda)*length(rho)) < nb.cpus)
      nb.cores <- (length(lambda)*length(rho))
    else
      nb.cores <- nb.cpus - 1 
  }
    
  
  wrapper.DiscriminantAnalysisGlasso <- function(prm)
  {
    result <- rcppDiscriminantAnalysisGlasso(data, knownlabels, nbCluster, prm[1], prm[2])
    return(result)
  }
  
  pen.grid <- matrix(0, (length(lambda)*length(rho)), 2)  
  pen.grid <- as.matrix(expand.grid(lambda, rho))
  pen.grid.list <- list(); colnames(pen.grid) <- NULL
  pen.grid.list <- as.list(data.frame(t(pen.grid)))
  parallel.varrole <-  mclapply(X = pen.grid.list, 
                                FUN = wrapper.DiscriminantAnalysisGlasso,
                                mc.cores = nb.cores,
                                mc.preschedule = TRUE,
                                mc.cleanup = TRUE )
 
  var.role <- matrix(0,(length(lambda)*length(rho)), p)
  for(j in 1:nrow(var.role))
    if(class(parallel.varrole[[j]])!="try-error")
      var.role[j,] <- parallel.varrole[[j]]   
  
  return(var.role)
}