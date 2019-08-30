logisticmodelreport <-function(testmodel,nullmodel){
  testmodel.modelChi<-testmodel$null.deviance-testmodel$deviance
  testmodel.chidf <- testmodel$df.null - testmodel$df.residual
  testmodel.chisq.prob <- 1 -pchisq(testmodel.modelChi,testmodel.chidf)

  cat("Pseudo R^2 for logistic regression","\n") 
  modelN <-length(testmodel$fitted.values)
  # Hosmer and Lemeshows R square
  R.hl<-testmodel.modelChi/testmodel$null.deviance
  cat("Hosmer and Lemeshow R^2    ", round(R.hl,3),"\n")
  # Cox and Snell R square
  R.cs <- 1- exp ((testmodel$deviance-testmodel$null.deviance)/modelN)
  cat("Cox and Snell R^2          ", round(R.cs,3),"\n")
  # Nagelkerke R square
  R.n <-R.cs/(1-(exp(-(testmodel$null.deviance/modelN))))
  cat("Nagelkerke R^2             ", round(R.n,3),"\n\n")

  
  cat("Comparing test model with null model (constant and subject only)","\n") 
  modelChi<-nullmodel$deviance-testmodel$deviance
  chidf <- nullmodel$df.residual -testmodel$df.residual
  chisq.prob <- 1 -pchisq(modelChi,chidf)
  cat("Chi square                 ", modelChi,"\n")
  cat("Chi square df              ", chidf,"\n")
  cat("Chi square prob            ", chisq.prob ,"\n\n")

}

