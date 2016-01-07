# check all inputs for SelvarClustLasso function
CheckInputsC <- function(x, nbcluster, lambda, rho, hsize, criterion, models, rmodel, imodel, nbcores)
{
  if(missing(x))
     stop("x is missing!")
       
   if(is.matrix(x) == FALSE && is.data.frame(x) == FALSE)
      stop(paste(sQuote("x"), "must be a matrix!"))
       
   if(missing(nbcluster))
      stop("nbcluster is missing!")
       
   if(sum(!is.wholenumber(nbcluster)))
      stop("nbcluster must contain only integer!")
       
   if(sum(nbcluster < 1))
      stop(paste(sQuote("nbcluster"), "must be an integer greater than 0!"))
       
   if(missing(lambda))
      stop("lambda is missing!")
       
   if(is.vector(lambda) == FALSE | length(lambda) <= 1) 
      stop(paste(sQuote("lambda"), "must be a vector with length >= 2!"))
       
   if(sum(lambda<=0))
      stop("lambda must be greater than 0!")
       
   if(missing(rho))
      stop("rho is missing!")
       
   if(is.vector(rho) == FALSE)
      stop(paste(sQuote("rho"), "must be a vector!"))
      
   if(sum(rho<=0))
      stop("rho must be greater than 0!")
       
   if(!is.wholenumber(hsize) | sum(hsize < 1) | hsize > ncol(x)) 
      stop(paste(sQuote("hsize"), "must be a positive integer <= ncol(x)!"))
       
   if(sum(criterion %in% c("BIC","ICL")) != length(criterion))
      stop(cat(criterion[which(!(criterion %in% c("BIC","ICL")))], "is not a valid criterion name !\n"))
       
   if(!(isS4(models) && is(models, "GaussianModel")))
      stop("models must be a GaussianModel S4 object! (see Rmixmod package)")
       
   if(sum(rmodel %in% c("LI","LB","LC")) != length(rmodel))
      stop(cat(rmodel[which(!(rmodel %in% c("LI","LB","LC")))], "is not a valid rmodel name !\n"))
       
   if(sum(imodel %in% c("LI","LB")) != length(imodel))
       stop(cat(imodel[which(!(imodel %in% c("LI","LB")))], "is not a valid imodel name !\n"))

}