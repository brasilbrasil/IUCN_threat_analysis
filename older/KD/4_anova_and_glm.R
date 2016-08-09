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


#####KD Model Testing ANOVA OVERDISPERSED
attach(all_data_combined)
model=aov(all_data_combined$n_threats ~ all_data_combined$CC_threat*all_data_combined$status*all_data_combined$Red.List.status*all_data_combined$Kingdom)
summary(model)

model1=update(model,~.-all_data_combined$CC_threat:all_data_combined$status:all_data_combined$Red.List.status:all_data_combined$Kingdom)
anova(model,model1)

#####KD Mpodel Testing GLM
modelGLM=glm(all_data_combined$n_threats ~ all_data_combined$CC_threat*all_data_combined$status*all_data_combined$Red.List.status*all_data_combined$Kingdom,quasipoisson)
summary(modelGLM)

##removed interaction 
modelGLM2=update(modelGLM,~.-all_data_combined$CC_threat:all_data_combined$status:all_data_combined$Red.List.status:all_data_combined$Kingdom)

##Test ANOVA signif (must leave in interaction = signif)
anova(modelGLM,modelGLM2,test="F")

##remove Kingdom parameter
modelGLM3=glm(all_data_combined$n_threats ~ all_data_combined$status*all_data_combined$Red.List.status*all_data_combined$Kingdom,quasipoisson)
summary(modelGLM3)
modelGLM4=update(modelGLM3,~.-all_data_combined$status:all_data_combined$Red.List.status:all_data_combined$Kingdom)
##Test ANOVA signif (must retain CC, but GEOG appears insignificant)
anova(modelGLM3,modelGLM4, test="F")

#model without GEOG
modelGLM5=glm(all_data_combined$n_threats ~ all_data_combined$CC_threat*all_data_combined$Red.List.status*all_data_combined$Kingdom,quasipoisson)
summary(modelGLM5)
modelGLM6=update(modelGLM5,~.-all_data_combined$CC_threat:all_data_combined$Red.List.status:all_data_combined$Kingdom)
anova(modelGLM5,modelGLM6,test="Chi")
summary(modelGLM6)

#Individual 1 dep and 2 indep variables
modelSTAT=glm(all_data_combined$n_threats ~ all_data_combined$CC_threat*all_data_combined$Red.List.status,poisson)
summary(modelSTAT) 

#poisson distribution retained - not overdispersed and test with test="Chi"

modelSTAT1=update(modelSTAT,~.-all_data_combined$CC_threat:all_data_combined$Red.List.status)
anova(modelSTAT,modelSTAT1,test="CHI")

modelGEOG=glm(all_data_combined$n_threats ~ all_data_combined$CC_threat*all_data_combined$status,poisson)
summary(modelGEOG)

modelGEOG1=update(modelGEOG,~.-all_data_combined$CC_threat:all_data_combined$status)
summary(modelGEOG1)
anova(modelGEOG,modelGEOG1,test="Chi")

modelKING=glm(all_data_combined$n_threats ~ all_data_combined$CC_threat*all_data_combined$Kingdom,poisson)
summary(modelKING)

modelKING1=update(modelKING,~.-all_data_combined$CC_threat:all_data_combined$status:all_data_combined$Red.List.status:all_data_combined$Kingdom)
anova(modelKING,modelKING1,test="Chi")
summary(modelKING1)
