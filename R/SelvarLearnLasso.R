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
           models = mixmodGaussianModel(listModels = "Gaussian_pk_Lk_C"),
           modelReg = c("LI", "LB", "LC"),
           modelIndep = c("LI", "LB"),
           knownlabels)
  {
    
    # check data parameter
    if(missing(data)){
      stop("data is missing !")
    } 
    if(is.matrix(data) == FALSE & is.data.frame(data) == FALSE) 
      stop(paste(sQuote("data"), "must be a matrix"))
    
    
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
      stop(paste(sQuote("lambda"), "must be a vector with length >= 2"))
    }
    if (sum(lambda<=0)){
      stop("lambda must greater than 0!")
    }
    
    
    
    # check rho parameter
    if(missing(rho)){
      stop("rho is missing!")
    }
    if(is.vector(rho) == FALSE){ 
      stop(paste(sQuote("rho"), "must be a vector"))
    }
    if(sum(rho<=0)){
      stop("rho must greater than 0!")
    }
     
    
    # check hybrid.size
    if(!is.wholenumber(hybrid.size) | sum(hybrid.size < 1) | hybrid.size > ncol(data)) 
      stop(paste(sQuote("hybrid.size"), "must be a positive integer <= ncol(data)"))
    
    # check criterion parameter
    if( sum(criterion %in% c("BIC")) != length(criterion) ){
      stop(cat(criterion[which(!(criterion %in% c("ICL")))], "is not a valid criterion name !\n"))
    }
    
    # check regModel
    if( sum(regModel %in% c("LI","LB","LC")) != length(regModel) ){
      stop(cat(regModel[which(!(regModel %in% c("LI","LB","LC")))], "is not a valid regModel name !\n"))
    }
    
    # check indepModel
    if ( sum(indepModel %in% c("LI","LB")) != length(indepModel) ){
      stop(cat(indepModel[which(!(indepModel %in% c("LI","LB")))], "is not a valid indepModel name !\n"))
    }
    
    # check whether the knownLabels is missing
    if ( length(knownlabels) == 0 ){
      stop("labels are missing!")
    }
    # check the number of cluster
    if (min(knownlabels) <= 0 | length(knownlabels) != nrow(data)){
      stop("Each observation in knownLabels must have a valid cluster affectation !")
    }
    # check the number of cluster
    if (length(nbCluster) != 1 | max(knownlabels)!= nbCluster ){
      warning("length of nbCluster must be 1 and equal to max(knownLabels)!")
      nbCluster <- max(knownlabels)
    }
    
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