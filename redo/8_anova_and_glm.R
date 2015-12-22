rm(list = ls()) #remove all past worksheet variables
library(reshape2)
library(ggplot2)
##run this code after merging all IUCN data

wd="D:/Dropbox/current work/IUCN_threat_analysis_redo/"
#wd="C:/Users/Kaipo Dye/Dropbox/IUCN_threat_analysis_redo/"
setwd(wd)
all_data_combined = read.csv(paste0("results/all_data_combined_onlySppWThreatInfo",".csv"), header = T, row.names = NULL, check.names = FALSE)

#all_data_combined_onlySppWThreatInfo have at least 1  threat (or else test will not be of augmentation)
#all_data_combined=all_data_combined[all_data_combined$n_nonCC_threat!=0,] <-- line removed(must have at least 1 threat) 


dependents="n_nonCC_threat"
data=all_data_combined

#0ne way anova 
independents=c("Kingdom")  ##n_nCC_threats ~ CC_threat/Red.List.status/Kingdom
int <- aov(as.formula(paste0(dependents, " ~ ", independents[1])), data=data)
summary(int)
plot(int)

#two way anova 
independents=c("CC_threat", "Red.List.status")
int <- aov(as.formula(paste0(dependents, " ~ ", independents[1],"*", independents[2])), data=data)
summary(int)
plot(int)

independents=c("CC_threat", "Kingdom")
int <- aov(as.formula(paste0(dependents, " ~ ", independents[1],"*", independents[2])), data=data)
summary(int)
plot(int)

#three way anova (with and without interaction)
independents=c("CC_threat", "Red.List.status", "Kingdom")
aov.with <- aov(as.formula(paste0(dependents, " ~ ", independents[1],"*", independents[2], "*", independents[3])), data=data)
#int <- aov(all_data_combined$n_threats ~ all_data_combined$CC_threat*all_data_combined$status)
summary(aov.with)
plot(aov.with)

#Investigate each interaction (cc_threat,Red.List.Status)
TukeyHSD(aov.with, conf.level=.99)

independents=c("CC_threat", "Red.List.status", "Kingdom")
aov.wout <- aov(as.formula(paste0(dependents, " ~ ", independents[1],"+", independents[2], "+", independents[3])), data=data)
#int <- aov(all_data_combined$n_threats ~ all_data_combined$CC_threat*all_data_combined$status)
summary(aov.wout)
plot(int)

#Chi-square test for interactivity legitimacy
anova(aov.with,aov.wout,test="Chi")

#1 factor GLM models
#glm and poisson family
#poison since dependent variable is count data
independents=c("CC_threat") ##n_nCC_threats ~ CC_threat/Red.List.status/Kingdom
glmFit=glm(as.formula(paste0(dependents, " ~ ", independents[1])), data=data, family=poisson())
summary(glmFit)
plot(glmFit)


#2 factor GLM models
#glm and poisson family
#poison since dependent variable is count data
independents=c("CC_threat") ## n_nCC_threats ~ CC_threat*Red.List.status/Kingdom
glmFit=glm(as.formula(paste0(dependents, " ~ ", independents[1],"*", independents[2])), data=data, family=poisson())
summary(glmFit)
plot(glmFit)

#3 factor GLM models
#glm and poisson family (with and without interaction)
#poison since dependent variable is count data
independents=c("CC_threat", "Red.List.status", "Kingdom")
glmFitW=glm(as.formula(paste0(dependents, " ~ ", independents[1],"*", independents[2],"*", independents[3])), data=data, family=poisson())
summary(glmFitW)
plot(glmFitW)

#no interactions
glmFitWO=glm(as.formula(paste0(dependents, " ~ ", independents[1]," + ", independents[2]," + ", independents[3])), data=data, family=poisson())
summary(glmFitWO)
plot((-)glmFitWO)

#CHI-square Interactivity test (Independence test)
anova(glmFitW,glmFitWO,test="Chi")

#Multi-FACTORIAL ANOVA
aov.out=aov(n_nonCC_threat ~ CC_threat*Red.List.status*Kingdom,data=all_data_combined)
summary(aov.out)
TukeyHSD(aov.out, conf.level=.99)

library(boot)
glm.diag=glm.diag(glmFitW)
glm.diag.plots(glmFit,glm.diag)
