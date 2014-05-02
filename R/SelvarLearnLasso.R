###################################################################################
##                               SelvarLearnLasso.R                              ##
###################################################################################
SelvarLearnLasso <- 
  function(data, 
           nbCluster, 
           lambda, 
           rho,
           pack.size = 3,  
           criterion = "BIC",
           models,
           modelReg = c("LI", "LB", "LC"),
           modelIndep = c("LI", "LB"),
           knownlabels)
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
    
    
    ## knownlabels
    if(is.vector(knownlabels) == FALSE | length(knownlabels) != nrow(data))
      stop(paste(sQuote("knownlabels"), "must be a vector the same length as the number of rows in", sQuote("data")))
    
    ## cohérence entre supervised, nbClust et knownlabels
    if(is.integer(knownlabels)==FALSE | min(knownlabels) <= 0)
      stop(paste(sQuote("knownlabels"), "must be a vector of integers from 1 to", sQuote("nbCluster")))
    
    
    if(length(nbCluster) != 1 | nbCluster != max(knownlabels))
    {
      
      if(length(nbCluster) != 1)
        warning(paste(sQuote("nbCluster"), dQuote("length(nbCluster) must be one")))
      if(nbCluster != max(knownlabels))
        warning(paste(sQuote("nbCluster"), dQuote("nbCluster must be equal to max(knownlabels)")))
      nbCluster <- max(knownlabels)
    }   
    
    
    ## pour criterion 
    if(criterion != "BIC")
      stop(paste(sQuote("criterion"), "must be only ", dQuote("BIC")))
    
    
    ## pour modelReg
    for(m in 1:length(modelReg))
      if(modelReg[m]!= "LI" & modelReg[m]!="LB" & modelReg[m]!="LC")
        stop(paste(sQuote("modelReg"), "must be one or more of", dQuote("LI"), "or", dQuote("LB"), "or", dQuote("LC")))
    
    ## pour modelIndep
    for(m in 1:length(modelIndep))
      if(modelIndep[m]!= "LI" & modelIndep[m]!="LB")
        stop(paste(sQuote("modelIndep"), "must be one or more of", dQuote("LI"), "or", dQuote("LB")))
    
    
    ## on est supervisé donc on initialiser supervised à TRUE.
    supervised <- TRUE
    ## penser au cas où l'utilisateur introduit une liste de matrices de données (une matrice par groupe)   
    data <- as.matrix(data)
    n <- as.integer(nrow(data))
    p <- as.integer(ncol(data))
    nbCluster <- as.integer(nbCluster)
    OrderVariable <- matrix(NA, nrow = length(nbCluster), ncol = p) 
    dataStand <- scale(data, TRUE, TRUE)
    print("............... start  variables  ranking .................................... ")
    OrderVariable <- SortVariablesLasso(dataStand,nbCluster,lambda, rho, supervised, knownlabels = knownlabels)
    print("................. variables ranking .... done ................................ ") 
    bestModel <- list()
    print(" ...... SRUW  selection with BIC criterion ...... ")
    VariableSelectRes <- VariableSelection(data,
                                           nbCluster,
                                           models,
                                           criterion,
                                           OrderVariable,
                                           hybrid.size,
                                           supervised,
                                           knownlabels)## ici les deux derniers arguements ne jouent qu'un rôle de création d'objet c++
    print(" ...... model selection with BIC criterion......")
    bestModel <- ModelSelectionClust(VariableSelectRes,
                                     data,
                                     regModel,
                                     indepModel)
    return(bestModel)  
    
  }