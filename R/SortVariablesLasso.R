################################################
##        SortVariablesLasso.R
################################################

##  INPUT
# data (dataframe)    matrix of data
# nbCluster (vector)  values of K
# lambda    format (vector)   
# rho       format (vector)    
## OUTPUT
#  OrderVariable = matrice de taille length(nbCluster) * p
# pour chaque ligne (corresp une valeur de K), on a l'ordre des p variables
SortVariablesLasso <- 
  function(data,
           nbCluster,
           lambda,
           rho,
           supervised = FALSE,
           knownlabels = NULL)
  {
    data <- as.matrix(scale(data, TRUE, TRUE))
    n <- as.integer(nrow(data))
    p <- as.integer(ncol(data))
    K <- as.integer(nbCluster)
    
    if(supervised == FALSE)
    {
    VarRole <- array(NA,dim=c((length(lambda)*length(rho)), p, length(nbCluster))) 
    VarRole <- ClusteringEMGlasso(data,nbCluster,lambda,rho)
    ## Calcul de la matrice O de taille length(nCluster) * p
    Matrix0 <- matrix(0, nrow=length(nbCluster), ncol=p)
    for (k in 1:length(nbCluster))
      Matrix0[k,]<- colSums(VarRole[,,k])    
    
    
    ## Ordre des variables
    OrderVariable <- matrix(NA, nrow=length(nbCluster),ncol=p)
    for (k in 1:length(nbCluster))
      OrderVariable[k,] <- sort.int(Matrix0[k,],decreasing=TRUE,index.return=TRUE)$ix
    }
    
    if(supervised==TRUE)
    {
     VarRole <- matrix(NA,(length(lambda)*length(rho)), p) 
     VarRole <- DiscriminantAnalysisGlasso(data,nbCluster, lambda, rho, knownlabels = knownlabels)
     var.role.sum <- colSums(VarRole) 
     OrderVariable <- sort.int(var.role.sum,decreasing=TRUE,index.return=TRUE)$ix
    }  
    return(OrderVariable)    
  }

  
  
  
  
  
  
  
  
