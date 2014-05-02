###################################################################################
##                               SelvarClustLasso.R                                     ##
###################################################################################
SelvarClustLasso <- 
  function(data, 
           nbCluster, 
           lambda, 
           rho,
           hybrid.size = 3,  
           criterion = "BIC",
           models,
           regModel = c("LI", "LB", "LC"),
           indepModel = c("LI", "LB"))
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
    
    
    ## pour hybrid.size
    if(round(hybrid.size) != hybrid.size | hybrid.size > ncol(data)) 
      stop(paste(sQuote("hybrid.size"), "must be an integer <= ncol(data)"))
    
    
    
    ## pour criterion 
    if(length(criterion)==1)
      if(criterion != "BIC" & criterion != "ICL")
        stop(paste(sQuote("criterion"), "must be one of", dQuote("BIC"), "or", dQuote("ICL"), "or", dQuote("c(BIC, ICL)")))
    
    if(length(criterion)==2  & ((criterion[1]!="BIC" | criterion[2]!="ICL" ) & (criterion[1]!="ICL" | criterion[2]!="BIC")))
      stop(paste(sQuote("criterion"), "must be one of", dQuote("BIC"), "or", dQuote("ICL"), "or", dQuote("c(BIC, ICL)")))
    
    
    
    
    ## pour modelReg
    for(m in 1:length(regModel))
      if(regModel[m]!= "LI" & regModel[m]!="LB" & regModel[m]!="LC")
        stop(paste(sQuote("regModel"), "must be one or more of", dQuote("LI"), "or", dQuote("LB"), "or", dQuote("LC")))
    
    ## pour modelIndep
    for(m in 1:length(indepModel))
      if(indepModel[m]!= "LI" & indepModel[m]!="LB")
        stop(paste(sQuote("indepModel"), "must be one or more of", dQuote("LI"), "or", dQuote("LB")))
    
    
    data <- as.matrix(data)
    n <- as.integer(nrow(data))
    p <- as.integer(ncol(data))
    nbCluster <- as.integer(nbCluster)
    OrderVariable <- matrix(NA, nrow = length(nbCluster), ncol = p) 
    dataStand <- scale(data, TRUE, TRUE)
    print("............... start  variables  ranking .................................... ")
    OrderVariable <- SortVariablesLasso(dataStand,nbCluster,lambda,rho)
    print("................. variables ranking .... done ................................ ")
    supervised <- FALSE ## c'est une initialisation qui ne sert qu'à créer l'objet CritClust en c++
    knownlabels <- as.integer(1:n) ## une initilialisation qui ne sert qu'à créer l'objet CritClust en c++  (une autre solution à trouver !!!)
    bestModel <- list()
    if(length(criterion)==1)
    {
      print(c("S,R, U and W selection for criterion", criterion))
      VariableSelectRes <- VariableSelection(data,
                                             nbCluster,
                                             models,
                                             criterion,
                                             OrderVariable,
                                             hybrid.size,
                                             supervised,
                                             knownlabels)## ici les deux derniers arguements ne jouent qu'un rôle de création d'objet c++
      if(criterion=="BIC")
        bestModel$BIC <- ModelSelectionClust(VariableSelectRes,
                                             data,
                                             regModel,
                                             indepModel)
      else
        bestModel$ICL <- ModelSelectionClust(VariableSelectRes,
                                             data,
                                             regModel,
                                             indepModel)
    }
    else
      for(crit in criterion)
      {
        print(c("S,R, U and W selection for criterion", crit))
        VariableSelectRes <- VariableSelection(data,
                                               nbCluster,
                                               models,
                                               crit,
                                               OrderVariable,
                                               hybrid.size,
                                               supervised,
                                               knownlabels)
        
        print(c(" ..... model selection  with ..... ", crit, " ..... criterion ....."))
        cmd <- paste('bestModel$', crit, ' <- ModelSelectionClust(VariableSelectRes,data,regModel,indepModel)', sep ="")
        eval(parse(text = cmd))
      }  
    
    return(bestModel) 
    
  }