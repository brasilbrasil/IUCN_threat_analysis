rm(list = ls()) #remove all past worksheet variables
library(reshape2)
library(ggplot2)
##run this code after merging all IUCN data

#wd="D:/Dropbox/current work/IUCN_threats_analysis_outputs/"
wd="C:/Users/Kaipo Dye/Dropbox/PICCC/IUCN_threats_analysis_outputs/"
setwd(wd)
all_data_combined = read.csv(paste0("resultsCheck/all_data_combined_onlySppWThreatInfo_wGeog",".csv"), header = T, row.names = NULL, check.names = FALSE)

#keep only species that have at least 1 non-climatic threat (or else test will not
#be of augmentation)
all_data_combined=all_data_combined[all_data_combined$n_nonCC_threat!=0,]  


dependents="n_nonCC_threat"
data=all_data_combined

#two way anova 
independents=c("CC_threat", "Red.List.status")
int <- aov(as.formula(paste0(dependents, " ~ ", independents[1],"*", independents[2])), data=data)
summary(int)
plot(int)

independents=c("CC_threat", "Kingdom")
int <- aov(as.formula(paste0(dependents, " ~ ", independents[1],"*", independents[2])), data=data)
summary(int)
plot(int)

#three way anova 
independents=c("CC_threat", "Red.List.status", "Kingdom")
inter <- aov(as.formula(paste0(dependents, " ~ ", independents[1],"*", independents[2], "*", independents[3])), data=data)
#int <- aov(all_data_combined$n_threats ~ all_data_combined$CC_threat*all_data_combined$status)
summary(inter)

addit <- aov(as.formula(paste0(dependents, " ~ ", independents[1],"+", independents[2], "+", independents[3])), data=data)
summary(addit)

anova(inter,addit,test="Chisq")
                              

#2 factor GLM models
#glm and poisson family
#poison since dependent variable is count data
independents=c("CC_threat", "Red.List.status")
glmFit=glm(as.formula(paste0(dependents, " ~ ", independents[1],"*", independents[2])), data=data, family=poisson())
summary(glmFit)
plot(glmFit)

#3 factor GLM models
#glm and poisson family
#poison since dependent variable is count data
independents=c("CC_threat", "Red.List.status", "Kingdom")
glmInt=glm(as.formula(paste0(dependents, " ~ ", independents[1],"*", independents[2],"*", independents[3])), data=data, family=poisson())
summary(glmInt)
plot(glmInt)

#no interactions
glmAdit=glm(as.formula(paste0(dependents, " ~ ", independents[1]," + ", independents[2]," + ", independents[3])), data=data, family=poisson())
summary(glmAdit)
plot(glmAdit)

anova(glmAdit,glmInt,test="Chisq")

library(boot)
glm.diag=glm.diag(glmFit)
glm.diag.plots(glmFit,glm.diag)