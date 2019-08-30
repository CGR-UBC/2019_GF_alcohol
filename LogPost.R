oddsratios<-exp(testmodel$coefficients)
modelforprint<-(summary(testmodel)$coefficients)
modelforprint<-data.frame(modelforprint)
oddsratios.frame<-data.frame(oddsratios)
modelforprint$or<-oddsratios.frame$oddsratios
orconfidenceintervals<-exp(confint(testmodel))
orconfidenceintervals.frame<-data.frame(orconfidenceintervals)
modelforprint$lci<-orconfidenceintervals.frame$X2.5..
modelforprint$uci<-orconfidenceintervals.frame$X97.5..


#diagnostocs
# prints out a file of diagnostics so we can see if there are points where the model
# fits poorly (anlayse resdiduals - only 5% values should lie outside +- 1.96, 1% +-2.53 - above 3 bad)
# or if points exert an undue influence on the model (DFeta, leverage - levergae shouuld be around 
# number of predictors +1 / sample size) This model1 = 39/7290 = 0.0053 - bad if two or three times as large as this.
data_for_analysis$predicted.probabilities<-fitted(testmodel)
data_for_analysis$standardised.residuals<-rstandard(testmodel)
data_for_analysis$studentised.residuals<-rstudent(testmodel)
data_for_analysis$dfbeta<-dfbeta(testmodel)
data_for_analysis$dffit<-dffits(testmodel)
data_for_analysis$leverage<-hatvalues(testmodel)

J=mean(data_for_analysis$leverage)*3

data_for_analysis$large.residuals<-data_for_analysis$standardised.residuals >2 | data_for_analysis$standardised.residuals < -2
cat("Number of residuals over +2 or - 2             ", sum(data_for_analysis$large.residuals),"\n\n")
large_residuals<-data_for_analysis[data_for_analysis$large.residuals,c("Subject","previous_feedback","limb", "group", "runlengthbinary", "streakbinary", "leverage","standardised.residuals")]

data_for_analysis$large.leverage<-data_for_analysis$leverage >J
cat("Number of levarages over threshold           ", sum(data_for_analysis$large.leverage),"\n\n")
large_leverage<-data_for_analysis[data_for_analysis$large.leverage,c("Subject","previous_feedback","limb", "group", "runlengthbinary", "streakbinary", "leverage","predicted.probabilities", "standardised.residuals")]
