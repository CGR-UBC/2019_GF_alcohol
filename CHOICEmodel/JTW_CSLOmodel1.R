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

data_for_analysis$Run.Length<-(data_for_analysis$Run.Length-1)

# test for linearity of the logit for continuous variables (run length) for model 1. Any 
# interaction taht is significant indicates that the main effects has violated the assumption of linearity of th logit
#/ Interaction term nees to be created for each continuous variable
cat("Test for linearity of the logit using CSLOmodel1.randomeffects.\nSignificant values for interaction term mean assumption has been violated.\n")
data_for_analysis$logRun.Lengthint<-log(data_for_analysis$Run.Length)*data_for_analysis$Run.Length
data_for_analysis$streak.Lengthint<-log(data_for_analysis$streak)*data_for_analysis$streak
testmodel.linearitytest<-glm(CSLO ~ Pairing +  Pairing:previous_feedback + Run.Length + logRun.Lengthint + streak + streak.Lengthint + previous_feedback + group,data=data_for_analysis,family=binomial())
summary(testmodel.linearitytest)

# # # # # # # # # # # # # #  FREQUENTIST ANALYSES # # # # # # # # # # # # # # # # 

# Model 1 (see readme). This is the model which is used for diagnostics/assumptions etc
testmodel<-glm(CSLO ~ Pairing  + Pairing:previous_feedback 
               + Run.Length
               + Run.Length:previous_feedback
               + Run.Length:group
               + Run.Length:previous_feedback:group
               + streakbinary
               + streakbinary:previous_feedback
               + streakbinary:group
               + streakbinary:previous_feedback:group
               -1
               ,data=data_for_analysis,family=binomial())
summary(testmodel)

# Model 2 (see readme).
testmodel2<-glm(CSLO ~ Pairing  + Pairing:previous_feedback 
               + runlengthbinary
               + runlengthbinary:previous_feedback
               + runlengthbinary:group
               + runlengthbinary:previous_feedback:group
               + streakbinary
               + streakbinary:previous_feedback
               + streakbinary:group
               + streakbinary:previous_feedback:group
               -1
               ,data=data_for_analysis,family=binomial())
summary(testmodel2)

# model with just subject and intercept for comparison
nullmodel<-glm(CSLO ~ Pairing + Pairing:previous_feedback, data=data_for_analysis,family=binomial())
summary(nullmodel)

# # # ASSESS MODEL
# calculate pseudo r2, chi-square for model
report<-logisticmodelreport(testmodel,nullmodel)

# calculate odds ratios, CI of odds ratios, diagnistics (leverage and residuals for all cases)
source(file="LogPost.R")

#percent over threshold
nrow(large_leverage)/nrow(data_for_analysis)*100
nrow(large_residuals)/nrow(data_for_analysis)*100

# # # # # EDIT filenames print model results to file # # # # 
write.table(modelforprint,"JTW1_CSLOmodel_results.csv",sep=",")
write.table(data_for_analysis,"JTW1_CSLOmodel1_data_with_diagnostics.csv",sep=",",row.names=FALSE)
write.table(large_residuals,"JTW1_CSLOmodel1_large_residuals.csv",sep=",",row.names=FALSE)
write.table(large_leverage,"JTW1_CSLOmodel_large_leverage.csv",sep=",",row.names=FALSE)

# # # # Predicted probabilities of chosen model # # # # 
# EDIT for factors you want included in predicted probabilities
 
pp<-expand.grid(Pairing=factor(Nlist),group=0:1,Run.Length=0:4,streakbinary=0:1,previous_feedback=0:1)
pp$predicted.probs<-predict(testmodel,pp,type="response")
write.table(pp,"JTW1_CSLOmodel_pp.csv",sep=",")

pp<-expand.grid(Pairing=factor(Nlist),previous_feedback=0:1)
pp$predicted.probs<-predict(nullmodel,pp,type="response")
write.table(pp,"JTW1_CSLOmodel_nullmodeltwosubjectterms_pp.csv",sep=",")

# c-statistic
x11()
plotROC(data_for_analysis,6,testmodel$fitted)


# # # # # # # # # # # # # #  BAYESIAN ANALYSES # # # # # # # # # # # # # # # # 
#Model3 (see readme)
testmodel3<-stan_glm(CSLO ~ Pairing  + Pairing:previous_feedback 
                + Run.Length
                + Run.Length:previous_feedback
                + Run.Length:group
                + Run.Length:previous_feedback:group
                + streakbinary
                + streakbinary:previous_feedback
                + streakbinary:group
                + streakbinary:previous_feedback:group
                -1,
                data=data_for_analysis,
                family=binomial(link="logit"), cores=2, iter=4000, warmup=1000,
                seed = 14124869)
summary(testmodel3)

#launch_shinystan(testmodel3)
prior_summary(testmodel3)
coefficients3<-coef(testmodel3)
intervals3<-posterior_interval(testmodel3,prob=0.95)

coefficients3
intervals3

plot(testmodel3, "ess")

# model 4  (see readme)
testmodel4<-stan_glm(CSLO ~ Pairing  + Pairing:previous_feedback 
                + runlengthbinary
                + runlengthbinary:previous_feedback
                + runlengthbinary:group
                + runlengthbinary:previous_feedback:group
                + streakbinary
                + streakbinary:previous_feedback
                + streakbinary:group
                + streakbinary:previous_feedback:group
                -1,
                data=data_for_analysis,
                family=binomial(link="logit"),cores=2, iter= 4000,
                seed = 144658309)
summary(testmodel4)

#launch_shinystan(testmodel3)
prior_summary(testmodel4)
coefficients4<-coef(testmodel4)
intervals4<-posterior_interval(testmodel4,prob=0.95)

coefficients4
intervals4

plot(testmodel4, "ess")