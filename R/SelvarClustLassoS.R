SelvarClustLassoS <- function(x, nbcluster, lambda, rho, hsize=3, criterion="BIC", models=mixmodGaussianModel(listModels = c("Gaussian_pk_L_C", "Gaussian_pk_Lk_C", "Gaussian_pk_L_Ck", "Gaussian_pk_Lk_Ck")), rmodel=c("LI", "LB", "LC"), imodel=c("LI", "LB"),nbcores= min(2,  detectCores(all.tests = FALSE, logical = FALSE)))
  {
    CheckInputsC(x, nbcluster, lambda, rho, hsize, criterion, models, rmodel, imodel, nbcores)
    reference <- BuildS4object1(x, nbcluster, lambda, rho, hsize, criterion, models, rmodel, imodel, nbcores, learn=FALSE, z=NULL, xt=NULL, zt=NULL)
    

    data <- as.matrix(data)
    n <- as.integer(nrow(data))
    p <- as.integer(ncol(data))
    nbCluster <- as.integer(nbCluster)
    OrderVariable <- matrix(NA, nrow = length(nbCluster), ncol = p) 
    dataStand <- scale(data, TRUE, TRUE)
    print("............... start  variable  ranking .................................... ")
    supervised <- FALSE ## c'est une initialisation qui ne sert qu'à créer l'objet CritClust en c++
    knownlabels <- as.integer(1:n) ## une initilialisation qui ne sert qu'à créer l'objet CritClust en c++  (une autre solution à trouver !!!)
    OrderVariable <- SortvarClust(dataStand, nbCluster, lambda, rho, nbCores)
    print("................. variable ranking .... done ................................ ")
    bestModel <- list()
    if(length(criterion)==1)
    {
      print(c(" ...... SRUW selection with...", criterion, "... criterion ......"))
      VariableSelectRes <- VariableSelection(data,
                                             nbCluster,
                                             models,
                                             criterion,
                                             OrderVariable,
                                             hybrid.size,
                                             supervised,
                                             knownlabels,
                                             nbCores)## ici les deux derniers arguements ne jouent qu'un rôle de création d'objet c++
      
      if(criterion=="BIC"){
        print(" ..... model selection  with BIC criterion...... ")
        bestModel$BIC <- ModelSelectionClust(VariableSelectRes,
                                             data,
                                             regModel,
                                             indepModel,
                                             nbCores)
      }
      else
      {
        print(" ..... model selection  with ICL criterion...... ")
        bestModel$ICL <- ModelSelectionClust(VariableSelectRes,
                                             data,
                                             regModel,
                                             indepModel,
                                             nbCores)
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
                                               knownlabels,
                                               nbCores)
        
        print(c(" ..... model selection  with ", crit, " criterion...... "))
        cmd <- paste('bestModel$', crit, ' <- ModelSelectionClust(VariableSelectRes,data,regModel,indepModel,nbCores)', sep ="")
        eval(parse(text = cmd))
      }  
    }
    
    
    return(bestModel) 
    
  }