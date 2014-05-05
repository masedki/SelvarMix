###################################################################################
##                               SelvarClustLasso.R                              ##
###################################################################################
SelvarClustLasso <- 
  function(data, 
           nbCluster, 
           lambda, 
           rho,
           hybrid.size = 3,  
           criterion = "BIC",
           models = mixmodGaussianModel(listModels = "Gaussian_pk_Lk_C"),
           regModel = c("LI", "LB", "LC"),
           indepModel = c("LI", "LB"))
  {
    
    # check data parameter
    if(missing(data)){
      stop("data is missing!")
    } 
    if(is.matrix(data) == FALSE && is.data.frame(data) == FALSE){ 
      stop(paste(sQuote("data"), "must be a matrix!"))
    }
    
    
    # check nbCluster parameter
    if(missing(nbCluster)){
      stop("nbCluster is missing!")
    }
    if(sum(!is.wholenumber(nbCluster))){
      stop("nbCluster must contain only integer!")
    }
    if(sum(nbCluster < 1)){ 
      stop(paste(sQuote("nbCluster"), "must be an integer greater than 0!"))
    }
    
    # check lambda parameter
    if(missing(lambda)){
      stop("lambda is missing!")
    } 
    if(is.vector(lambda) == FALSE | length(lambda) <= 1){ 
      stop(paste(sQuote("lambda"), "must be a vector with length >= 2!"))
    }
    if (sum(lambda<=0)){
      stop("lambda must greater than 0!")
    }
    
    
    # check rho parameter
    if(missing(rho)){
      stop("rho is missing!")
    } 
    if(is.vector(rho) == FALSE){ 
      stop(paste(sQuote("rho"), "must be a vector!"))
    }
    if(sum(rho<=0)){
      stop("rho must greater than 0!")
    }
    
    
    # check hybrid.size parameter
    if(!is.wholenumber(hybrid.size) | sum(hybrid.size < 1) | hybrid.size > ncol(data)) 
      stop(paste(sQuote("hybrid.size"), "must be a positive integer <= ncol(data)!"))
    
    # check criterion parameter
    if( sum(criterion %in% c("BIC","ICL")) != length(criterion) ){
      stop(cat(criterion[which(!(criterion %in% c("BIC","ICL")))], "is not a valid criterion name !\n"))
    }
    
    
    # check regModel
    if( sum(regModel %in% c("LI","LB","LC")) != length(regModel) ){
      stop(cat(regModel[which(!(regModel %in% c("LI","LB","LC")))], "is not a valid regModel name !\n"))
    }
    
    # check indepModel
    if ( sum(indepModel %in% c("LI","LB")) != length(indepModel) ){
      stop(cat(indepModel[which(!(indepModel %in% c("LI","LB")))], "is not a valid indepModel name !\n"))
    }
    
    
    data <- as.matrix(data)
    n <- as.integer(nrow(data))
    p <- as.integer(ncol(data))
    nbCluster <- as.integer(nbCluster)
    OrderVariable <- matrix(NA, nrow = length(nbCluster), ncol = p) 
    dataStand <- scale(data, TRUE, TRUE)
    print("............... start  variable  ranking .................................... ")
    OrderVariable <- SortVariablesLasso(dataStand,nbCluster,lambda,rho)
    print("................. variable ranking .... done ................................ ")
    supervised <- FALSE ## c'est une initialisation qui ne sert qu'à créer l'objet CritClust en c++
    knownlabels <- as.integer(1:n) ## une initilialisation qui ne sert qu'à créer l'objet CritClust en c++  (une autre solution à trouver !!!)
    bestModel <- list()
    if(length(criterion)==1)
    {
      print(c(" ...... SRUW selection with ", criterion, " criterion ...... "))
      VariableSelectRes <- VariableSelection(data,
                                             nbCluster,
                                             models,
                                             criterion,
                                             OrderVariable,
                                             hybrid.size,
                                             supervised,
                                             knownlabels)## ici les deux derniers arguements ne jouent qu'un rôle de création d'objet c++
      if(criterion=="BIC"){
        print(" ..... model selection  with BIC criterion...... ")
        bestModel$BIC <- ModelSelectionClust(VariableSelectRes,
                                             data,
                                             regModel,
                                             indepModel)
      }
      else
      {
        print(" ..... model selection  with ICL criterion...... ")
        bestModel$ICL <- ModelSelectionClust(VariableSelectRes,
                                             data,
                                             regModel,
                                             indepModel)
      }
    }
    else
    {
      for(crit in criterion)
      {
        print(c(" ...... SRUW selection with ", crit, " criterion...... "))
        VariableSelectRes <- VariableSelection(data,
                                               nbCluster,
                                               models,
                                               crit,
                                               OrderVariable,
                                               hybrid.size,
                                               supervised,
                                               knownlabels)
        
        print(c(" ..... model selection  with ", crit, " criterion...... "))
        cmd <- paste('bestModel$', crit, ' <- ModelSelectionClust(VariableSelectRes,data,regModel,indepModel)', sep ="")
        eval(parse(text = cmd))
      }  
    }
return(bestModel) 

  }