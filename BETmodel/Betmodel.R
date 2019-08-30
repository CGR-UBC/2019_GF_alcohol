library(ggplot2)
library(car)
library(rstanarm)
library(shinystan)
library(bayesplot)
library (PredictABEL)

# # # # # # # # # THINGS TO MODIFY # # # # # # # # # # # # # 
source(file="LogModel.R")
setwd("***") # set
data=read.csv("***.csv") #modify
N=38 # number of participants
Nlist=(c( )) # create list of participants


# # # # # # # # # DATA PREP # # # # # # # # #  # # # # 
source(file="LogPrep.R")
J=(84/(nrow(data_for_analysis)))*3
options(digits=10)


# test for linearity of the logit for continuous variables (run length) for model 1. Any 
# interaction taht is significant indicates that the main effects has violated the assumption of linearity of th logit
#/ Interaction term nees to be created for each continuous variable
cat("Test for linearity of the logit using CSLOmodel1.randomeffects.\nSignificant values for interaction term mean assumption has been violated.\n")
data_for_analysis$logRun.Lengthint<-log(data_for_analysis$Run.Length)*data_for_analysis$Run.Length
data_for_analysis$streak.Lengthint<-log(data_for_analysis$streak)*data_for_analysis$streak
testmodel.linearitytest<-glm(Bigbet.median.split ~ Pairing + Run.Length + logRun.Lengthint + streak + streak.Lengthint + previous_feedback + group + CSLO,data=data_for_analysis,family=binomial())
summary(testmodel.linearitytest)

data_for_analysis$streak<-(data_for_analysis$streak-1)
data_for_analysis$Run.Length<-(data_for_analysis$Run.Length-1)

# # # # # # # # # # # # # #  ANALYSES # # # # # # # # # # # # # # # # 

# this includes everything
fullmodel<-glm(Bigbet.median.split~ Pairing 
               + CSLO
               + Run.Length
               + previous_feedback 
               + streak
               + group
               + CSLO:Run.Length    
               + CSLO:streak
               + Run.Length:streak  
               + CSLO:previous_feedback 
               + Run.Length:previous_feedback 
               + streak:previous_feedback 
               + CSLO:group 
               + Run.Length:group
               + streak:group 
               + previous_feedback:group  
               + CSLO:Run.Length:streak  
               + CSLO:Run.Length:previous_feedback  
               + CSLO:streak:previous_feedback
               + Run.Length:streak:previous_feedback
               + CSLO:Run.Length:group  
               + CSLO:streak:group  
               + Run.Length:streak:group 
               + CSLO:previous_feedback:group  
               + Run.Length:previous_feedback:group
               + streak:previous_feedback:group 
               + CSLO:Run.Length:streak:previous_feedback
               + CSLO:Run.Length:streak:group  
               + CSLO:Run.Length:previous_feedback:group 
               + CSLO:streak:previous_feedback:group 
               + Run.Length:streak:previous_feedback:group
               + CSLO:Run.Length:streak:previous_feedback:group,data=data_for_analysis,family=binomial())
summary(fullmodel)

#newmodel - model 1 use for dignostics
testmodel<-glm(Bigbet.median.split~ Pairing 
               + CSLO
               + Run.Length
               + previous_feedback 
               + streak 
               + CSLO:streak
               + CSLO:previous_feedback 
               + streak:previous_feedback 
               + previous_feedback:group
               -1,data=data_for_analysis,family=binomial())
summary(testmodel)

# model with just subject and intercept for comparison
nullmodel<-glm(Bigbet.median.split ~ Pairing, data=data_for_analysis,family=binomial())
summary(nullmodel)

# model 2 - linear predictors enetered as binary predictors
testmodel2<-glm(Bigbet.median.split ~ Pairing 
                + runlengthbinary         
                + previous_feedback     
                + CSLO         
                + streakbinary
                + CSLO:streakbinary
                +CSLO:previous_feedback
                + streakbinary:previous_feedback
                + group:previous_feedback
                -1,data=data_for_analysis,family=binomial())

summary(testmodel2)

# # # ASSESS MODEL
# calculate pseudo r2, chi-square for model
report<-logisticmodelreport(testmodel,nullmodel)

# calculate odds ratios, CI of odds ratios, diagnistics (leverage and residuals for all cases)
source(file="LogPost.R")

#percent over threshold
nrow(large_leverage)/nrow(data_for_analysis)*100
nrow(large_residuals)/nrow(data_for_analysis)*100

# # # # # EDIT filenames print model results to file # # # # 
write.table(modelforprint,"JTW1_Betmodel_results.csv",sep=",")
write.table(data_for_analysis,"JTW1_Betmodel_data_with_diagnostics.csv",sep=",",row.names=FALSE)
write.table(large_residuals,"JTW1_Betmodel_large_residuals.csv",sep=",",row.names=FALSE)
write.table(large_leverage,"JTW1_Betmodel_large_leverage.csv",sep=",",row.names=FALSE)

# # # # Predicted probabilities of chosen model # # # # 
# EDIT for factors you want included in predicted probabilities

pp<-expand.grid(Pairing=factor(Nlist),group=0:1,Run.Length=1:5,streak=1:5,previous_feedback=0:1, CSLO=0:1)
pp$predicted.probs<-predict(testmodel,pp,type="response")
write.table(pp,"JTW1_Betmodel.csv",sep=",")


pp<-expand.grid(Pairing=factor(Nlist),previous_feedback=0:1)
pp$predicted.probs<-predict(nullmodel,pp,type="response")
write.table(pp,"JTW1_Betmodel_nullmodeltwosubjectterms_pp.csv",sep=",")

library ("PredictABEL")

# c-statistic
x11()
plotROC(data_for_analysis,11,testmodel$fitted)

# model 3 - bayesian version of model 1
testmodel3<-stan_glm(Bigbet.median.split ~ Pairing 
                     + Run.Length           
                     + previous_feedback     
                     + CSLO         
                     + Streak.Length  
                     + CSLO:Streak.Length
                     +CSLO:previous_feedback
                     + Streak.Length:previous_feedback
                     + group:previous_feedback
                     -1,
                     data=data_for_analysis,
                     family=binomial(link="logit"), cores=2, iter=4000, warmup=1000,
                     seed = 1121424869)
summary(testmodel3)

#launch_shinystan(testmodel3)
prior_summary(testmodel3)
coefficients3<-coef(testmodel3)
intervals3<-posterior_interval(testmodel3,prob=0.95)

coefficients3
intervals3

# model 3 - bayesian version of model 2
testmodel4<-stan_glm(Bigbet.median.split ~ Pairing 
                     + runlengthbinary         
                     + previous_feedback     
                     + CSLO         
                     + streakbinary
                     + CSLO:streakbinary
                     +CSLO:previous_feedback
                     + streakbinary:previous_feedback
                     + group:previous_feedback
                     -1,
                     data=data_for_analysis,
                     family=binomial(link="logit"),cores=2, iter= 4000,
                     seed = 144653709)
summary(testmodel4)

#launch_shinystan(testmodel3)
prior_summary(testmodel4)
coefficients4<-coef(testmodel4)
intervals4<-posterior_interval(testmodel4,prob=0.95)

coefficients4
intervals4


plot(M1_stanlmer, "ess")




