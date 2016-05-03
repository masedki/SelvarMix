selvarlasso <- function(x, nbcluster, lambda=seq(20, 100, by = 10), rho=seq(1, 2, length=2), hsize=3, criterion="BIC", models=mixmodGaussianModel(listModels = c("Gaussian_pk_L_C", "Gaussian_pk_Lk_C", "Gaussian_pk_L_Ck", "Gaussian_pk_Lk_Ck")), rmodel=c("LI", "LB", "LC"), imodel=c("LI", "LB"),nbcores= min(2,  detectCores(all.tests = FALSE, logical = FALSE)))
{
  CheckInputsC(x, nbcluster, lambda, rho, hsize, criterion, models, rmodel, imodel, nbcores)
  reference <- BuildS4object1(x, nbcluster, lambda, rho, hsize, criterion, models, rmodel, imodel, nbcores, learn=FALSE, z=numeric(), xt=matrix(0,0,0), zt=numeric())
  #print("...... start  variable  ranking ......")
  #reference <- clustemglasso(reference)
  reference@model@rank <- rbind(1:ncol(x), 1:ncol(x))
  #print("................. variable ranking .... done ................................ ")
  #print(reference@model@rank)
  varselres <- varsel(reference, criterion, learn=FALSE)
  #return(varselres)
  reference <- modelsel(varselres, criterion, reference)
  return(reference) 
  
}
