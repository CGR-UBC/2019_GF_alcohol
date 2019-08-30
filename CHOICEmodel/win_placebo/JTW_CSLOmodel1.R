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
options(digits=10)

data_for_analysis$Run.Length<-(data_for_analysis$Run.Length-1)
data_for_analysis<-subset(data_for_analysis,previous_feedback==1)
data_for_analysis<-subset(data_for_analysis,group==0)

# # # # # # # # # # # # # #  ANALYSES # # # # # # # # # # # # # # # # 

# eve's new hypothesis driven model
testmodel<-glm(CSLO ~ Pairing 
               + Run.Length
               + streakbinary
               -1
               ,data=data_for_analysis,family=binomial())
summary(testmodel)

data_for_analysis_trim<-subset(data_for_analysis,Run.Length<4)
testmodel2<-glm(CSLO ~ Pairing 
               + runlengthbinary
               + streakbinary
               -1
               ,data=data_for_analysis,family=binomial())
summary(testmodel2)

# model 3 (bayesian version of model 1)
testmodel3<-stan_glm(CSLO ~ Pairing  
                + Run.Length
                + streakbinary
                -1,
                data=data_for_analysis,
                family=binomial(link="logit"),cores=2, iter= 4000,
                seed = 23424869)
summary(testmodel3)

#launch_shinystan(testmodel3)
prior_summary(testmodel3)
coefficients3<-coef(testmodel3)
intervals3<-posterior_interval(testmodel3,prob=0.95)

coefficients3
intervals3

# model 4 (bayesian version of model 2)
testmodel4<-stan_glm(CSLO ~ Pairing  
                + runlengthbinary
                + streakbinary
                -1,
                data=data_for_analysis,
                family=binomial(link="logit"),cores=2, iter= 4000,
                seed = 23412869)
summary(testmodel4)

#launch_shinystan(testmodel4)
prior_summary(testmodel4)
coefficients4<-coef(testmodel4)
intervals4<-posterior_interval(testmodel4,prob=0.95)

coefficients4
intervals4