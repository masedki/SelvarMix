###################################################################################
##                               SelvarMix.R                                     ##
###################################################################################
SelvarMix <- 
  function(data, 
           nbCluster, 
           lambda, 
           rho,
           pack.size = 3,  
           criterion = "BIC",
           models,
           modelReg = c("LI", "LB", "LC"),
           modelIndep = c("LI", "LB"),
           supervised = FALSE,
           knownlabels = NULL)
  {
    
    ## tests sur les arguments de la fonction 
    ## pour data
    if(is.matrix(data) == FALSE & is.data.frame(data) == FALSE) 
      stop(paste(sQuote("data"), "must be a matrix"))
    
    
    ## pour nbCluster
    if(is.integer(as.integer(nbCluster))== FALSE) 
      stop(paste(sQuote("nbCluster"), "must be an integer"))
    
    
    ## pour lambda
    if(is.vector(lambda) == FALSE | length(lambda) <= 1) 
      stop(paste(sQuote("lambda"), "must be a vector with length >= 2"))
    
    
    ## pour rho
    if(is.vector(rho) == FALSE) 
      stop(paste(sQuote("rho"), "must be a vector"))
    
    
    ## pour pack.size
    if(round(pack.size) != pack.size | pack.size > ncol(data)) 
      stop(paste(sQuote("pack.size"), "must be an integer <= ncol(data)"))
    
    
    ## pour supervised et knownlabels
    if(is.logical(supervised) == FALSE)
      stop(paste(sQuote("supervised"), "must be", dQuote("TRUE"), "or", dQuote("FALSE"), ". Default is", dQuote("FALSE")))
    
    if((supervised == TRUE))  
      if(is.vector(knownlabels) == FALSE | length(knownlabels) != nrow(data))
        stop(paste(sQuote("knownlabels"), "must be a vector the same length as the number of rows in", sQuote("data")))
    
    ## cohérence entre supervised, nbClust et knownlabels
    if(supervised == TRUE)  
      if(is.integer(knownlabels)==FALSE | min(knownlabels) <= 0)
        stop(paste(sQuote("knownlabels"), "must be a vector of integers from 1 to", sQuote("nbCluster")))
    
    
    if(supervised == TRUE) 
      if(length(nbCluster) != 1 | nbCluster != max(knownlabels))
      {
        
        if(length(nbCluster) != 1)
          warning(paste(sQuote("nbCluster"), dQuote("length(nbCluster) must be one")))
        if(nbCluster != max(knownlabels))
          warning(paste(sQuote("nbCluster"), dQuote("nbCluster must be equal to max(knownlabels)")))
        nbCluster <- max(knownlabels)
      }   
    
    if(supervised == FALSE)  
      if(is.null(knownlabels) == FALSE)
        warning("supervised = FALSE, knownlabels was not used")
    
    
    ## pour criterion 
    if(length(criterion)==1 & supervised == FALSE)
      if(criterion != "BIC" & criterion != "ICL")
        stop(paste(sQuote("criterion"), "must be one of", dQuote("BIC"), "or", dQuote("ICL"), "or", dQuote("c(BIC, ICL)")))
    
    if(length(criterion)==2 & supervised == FALSE & ((criterion[1]!="BIC" | criterion[2]!="ICL" ) & (criterion[1]!="ICL" | criterion[2]!="BIC")))
      stop(paste(sQuote("criterion"), "must be one of", dQuote("BIC"), "or", dQuote("ICL"), "or", dQuote("c(BIC, ICL)")))
    
    if(supervised == TRUE & length(criterion) != 1) 
      stop(paste(sQuote("criterion (supervised case)"), "must be", dQuote("BIC")))
    
    if(supervised == TRUE) 
      if(criterion != "BIC") 
        stop(paste(sQuote("criterion (supervised case)"), "must be", dQuote("BIC")))
    
    
    ## pour modelReg
    for(m in 1:length(modelReg))
      if(modelReg[m]!= "LI" & modelReg[m]!="LB" & modelReg[m]!="LC")
        stop(paste(sQuote("modelReg"), "must be one or more of", dQuote("LI"), "or", dQuote("LB"), "or", dQuote("LC")))
    
    ## pour modelIndep
    for(m in 1:length(modelIndep))
      if(modelIndep[m]!= "LI" & modelIndep[m]!="LB")
        stop(paste(sQuote("modelIndep"), "must be one or more of", dQuote("LI"), "or", dQuote("LB")))
    
    
    ## cas non supervisé
    if(supervised == FALSE)
    {
      data <- as.matrix(data)
      n <- as.integer(nrow(data))
      p <- as.integer(ncol(data))
      nbCluster <- as.integer(nbCluster)
      OrderVariable <- matrix(NA, nrow = length(nbCluster), ncol = p) 
      dataStand <- scale(data, TRUE, TRUE)
      OrderVariable <- SortVariablesLasso(dataStand,nbCluster,lambda,rho)
      if(length(nbCluster)==1)
      {
        print(c("nbCluster = .... ", nbCluster))
        print(c("variables ranking = .... ", OrderVariable))
      }
      else
        for(k in 1:length(nbCluster))
        {
          print(c("nbCluster = .... ", nbCluster[k]))
          print(c("variables ranking = .... ", OrderVariable[k,]))  
          
        }
      ## 
      if(supervised == FALSE)
        knownlabels <- c(1,2,3) ## une initilialisation qui ne sert qu'à créer l'objet CritClust en c++  (une autre solution à trouver !!!)
      
      VariableSelectRes <- VariableSelection(data,nbCluster,models,modelReg,criterion,OrderVariable,pack.size, supervised, knownlabels)
      BestModels <- ModelSelectionClust(VariableSelectRes, data, nbCluster, models, modelReg, modelIndep, criterion) 
    }
    ## cas supervisé
    if(supervised == TRUE)
    {
      ## penser au cas où l'utilisateur introduit une liste de matrices de données (une matrice par groupe)   
      data <- as.matrix(data)
      n <- as.integer(nrow(data))
      p <- as.integer(ncol(data))
      nbCluster <- as.integer(nbCluster)
      OrderVariable <- matrix(NA, nrow = length(nbCluster), ncol = p) 
      dataStand <- scale(data, TRUE, TRUE)
      OrderVariable <- SortVariablesLasso(dataStand,nbCluster,lambda,rho, supervised = TRUE, knownlabels = knownlabels)
      print(c("nbCluster = .... ", nbCluster))
      print(c("variables ranking = .... ", OrderVariable)) 
      VariableSelectRes <- VariableSelection(data,nbCluster,models,modelReg,criterion,OrderVariable,pack.size, supervised, knownlabels)
      BestModels <- ModelSelectionClust(VariableSelectRes, data, nbCluster, models, modelReg, modelIndep, criterion)
    }
    return(BestModels)  
    
  }